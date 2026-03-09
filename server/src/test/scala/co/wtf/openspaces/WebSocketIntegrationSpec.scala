package co.wtf.openspaces

import co.wtf.openspaces.auth.{AdminConfig, AuthenticatedTicketService}
import co.wtf.openspaces.discussions.{Discussion, DiscussionAction, DiscussionActionConfirmed, DiscussionDataStore, DiscussionState, DiscussionStore, SchedulingService}
import co.wtf.openspaces.github.GitHubProfileService
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.lightning_talks.LightningTalkService
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.activities.*
import neotype.unwrap
import zio.*
import zio.direct.*
import zio.http.*
import zio.http.ChannelEvent.UserEvent
import zio.json.*
import zio.schema.codec.JsonCodec.zioJsonBinaryCodec
import zio.test.*

/** Focused WebSocket integration tests using ZIO HTTP's TestClient.
  *
  * Tests verify the key behaviors of the WebSocket server:
  * - Authentication flow (ticket → state sync)
  * - Unauthorized action rejection
  * - Multi-client state synchronization
  * - Connection lifecycle management
  */
object WebSocketIntegrationSpec extends ZIOSpecDefault:

  // Test helper: client channel wrapper for sending typed messages
  private case class TestClientChannel(channel: WebSocketChannel):
    def send(message: WebSocketMessageFromClient): Task[Unit] =
      channel.send(ChannelEvent.Read(WebSocketFrame.text(message.toJson)))

  // Test helper: collect received messages into a Ref
  private def makeCollectingClient(
    messagesRef: Ref[Vector[WebSocketMessageFromServer]],
    onHandshake: TestClientChannel => Task[Unit] = _ => ZIO.unit,
  ): WebSocketApp[Any] =
    Handler.webSocket { channel =>
      val testChannel = TestClientChannel(channel)
      channel.receiveAll {
        case ChannelEvent.Read(WebSocketFrame.Text(text)) =>
          text.fromJson[WebSocketMessageFromServer] match
            case Right(msg) => messagesRef.update(_ :+ msg)
            case Left(_)    => ZIO.unit

        case ChannelEvent.UserEventTriggered(UserEvent.HandshakeComplete) =>
          onHandshake(testChannel)

        case _ => ZIO.unit
      }
    }

  // Shared test setup: installs WebSocket app and ticket routes
  private val setupTestEnvironment: ZIO[
    BackendSocketApp & TicketRoutesApp & TestClient,
    Nothing,
    Unit,
  ] =
    for
      app <- ZIO.service[BackendSocketApp]
      ticketRoutes <- ZIO.service[TicketRoutesApp]
      _ <- TestClient.installSocketApp(app.socketApp)
      _ <- TestClient.addRoutes(ticketRoutes.routes)
    yield ()

  // Helper: wait for async message delivery
  private def waitForDelivery: UIO[Unit] =
    ZIO.withClock(Clock.ClockLive)(ZIO.sleep(300.millis))

  // Helper: obtain a valid ticket
  private def getTicket: ZIO[Client & Scope, Throwable, Ticket] =
    for
      client <- ZIO.service[Client]
      response <- client(
        Request
          .get(URL.root / "ticket")
          .addHeader(Header.Authorization.Bearer("test-token")),
      )
      ticket <- response.body.to[Ticket]
    yield ticket

  override def spec =
    suite("WebSocket Integration")(
      test("authenticated client receives initial state on connect") {
        defer:
          setupTestEnvironment.run
          val client = ZIO.service[Client].run
          val ticket = getTicket.run

          val receivedMessages = Ref.make(Vector.empty[WebSocketMessageFromServer]).run

          val clientApp = makeCollectingClient(
            receivedMessages,
            onHandshake = channel => channel.send(ticket),
          )

          client.socket(clientApp).run
          waitForDelivery.run

          val messages = receivedMessages.get.run
          

          // Should receive StateReplace messages for all entity types
          val hasDiscussionState = messages.exists {
            case DiscussionActionConfirmedMessage(
                  DiscussionActionConfirmed.StateReplace(_, _),
                ) =>
              true
            case _ => false
          }
          val hasLightningState = messages.exists {
            case LightningTalkActionConfirmedMessage(
                  LightningTalkActionConfirmed.StateReplace(_),
                ) =>
              true
            case _ => false
          }
          val hasHackathonState = messages.exists {
            case HackathonProjectActionConfirmedMessage(
                  HackathonProjectActionConfirmed.StateReplace(_),
                ) =>
              true
            case _ => false
          }

          assertTrue(
            hasDiscussionState,
            hasLightningState,
            hasHackathonState,
          )
      },
      test("unauthenticated client receives Unauthorized for actions") {
        defer:
          setupTestEnvironment.run
          val client = ZIO.service[Client].run

          val receivedMessages = Ref.make(Vector.empty[WebSocketMessageFromServer]).run

          // Send action WITHOUT sending ticket first
          val clientApp = makeCollectingClient(
            receivedMessages,
            onHandshake = channel =>
              channel.send(
                DiscussionActionMessage(
                  DiscussionAction.Add(
                    Topic.unsafeMake("Unauthorized Test Topic"),
                    Person("TestUser"),
                  ),
                ),
              ),
          )

          client.socket(clientApp).run
          waitForDelivery.run

          val messages = receivedMessages.get.run

          // Should receive Unauthorized response
          val hasUnauthorized = messages.exists {
            case DiscussionActionConfirmedMessage(
                  DiscussionActionConfirmed.Unauthorized(_),
                ) =>
              true
            case _ => false
          }

          assertTrue(hasUnauthorized)
      },
      test("ticketed but unapproved actor receives Unauthorized for actions") {
        defer:
          setupTestEnvironment.run
          val client = ZIO.service[Client].run
          val ticket = getTicket.run

          val receivedMessages = Ref.make(Vector.empty[WebSocketMessageFromServer]).run

          val clientApp = makeCollectingClient(
            receivedMessages,
            onHandshake = channel =>
              for
                _ <- channel.send(ticket)
                _ <- ZIO.withClock(Clock.ClockLive)(ZIO.sleep(100.millis))
                _ <- channel.send(
                  DiscussionActionMessage(
                    DiscussionAction.Add(
                      Topic.unsafeMake("Pending user should be blocked"),
                      Person("pending-user"),
                    ),
                  ),
                )
              yield (),
          )

          client.socket(clientApp).run
          waitForDelivery.run

          val messages = receivedMessages.get.run
          val hasUnauthorized = messages.exists {
            case DiscussionActionConfirmedMessage(DiscussionActionConfirmed.Unauthorized(_)) => true
            case _ => false
          }
          val hasAddResult = messages.exists {
            case DiscussionActionConfirmedMessage(DiscussionActionConfirmed.AddResult(_)) => true
            case _ => false
          }

          assertTrue(hasUnauthorized, !hasAddResult)
      },
      test("authenticated client can add discussion and receives confirmation") {
        defer:
          setupTestEnvironment.run
          val client = ZIO.service[Client].run
          val ticket = getTicket.run

          val receivedMessages = Ref.make(Vector.empty[WebSocketMessageFromServer]).run
          val handshakeComplete = Promise.make[Nothing, Unit].run

          val clientApp = makeCollectingClient(
            receivedMessages,
            onHandshake = channel =>
              for
                _ <- channel.send(ticket)
                // Small delay to ensure authentication completes
                _ <- ZIO.withClock(Clock.ClockLive)(ZIO.sleep(100.millis))
                _ <- channel.send(
                  DiscussionActionMessage(
                    DiscussionAction.Add(
                      Topic.unsafeMake("Test Discussion Topic"),
                      Person("TestFacilitator"),
                    ),
                  ),
                )
                _ <- handshakeComplete.succeed(())
              yield (),
          )

          client.socket(clientApp).run
          handshakeComplete.await.run
          waitForDelivery.run

          val messages = receivedMessages.get.run

          // Should receive AddResult confirmation
          val addResult = messages.collectFirst {
            case DiscussionActionConfirmedMessage(
                  DiscussionActionConfirmed.AddResult(discussion),
                ) =>
              discussion
          }

          assertTrue(
            addResult.isDefined,
            addResult.get.topic.unwrap == "Test Discussion Topic",
            addResult.get.facilitator.unwrap == "TestFacilitator",
          )
      },
      test("multiple clients see consistent state after action") {
        def extractAddResults(
          msgs: Vector[WebSocketMessageFromServer],
        ): Vector[Discussion] =
          msgs.collect {
            case DiscussionActionConfirmedMessage(
                  DiscussionActionConfirmed.AddResult(d),
                ) =>
              d
          }

        defer:
          setupTestEnvironment.run
          val client = ZIO.service[Client].run

          // Get tickets for both clients
          val ticket1 = getTicket.run
          val ticket2 = getTicket.run

          val client1Messages = Ref.make(Vector.empty[WebSocketMessageFromServer]).run
          val client2Messages = Ref.make(Vector.empty[WebSocketMessageFromServer]).run

          val client1Ready = Promise.make[Nothing, Unit].run
          val client2Ready = Promise.make[Nothing, Unit].run
          val actionSent = Promise.make[Nothing, Unit].run

          // Client 1: authenticates and adds a discussion
          val client1App = makeCollectingClient(
            client1Messages,
            onHandshake = channel =>
              for
                _ <- channel.send(ticket1)
                _ <- ZIO.withClock(Clock.ClockLive)(ZIO.sleep(100.millis))
                _ <- client1Ready.succeed(())
                // Wait for client 2 to be ready before sending action
                _ <- client2Ready.await
                _ <- channel.send(
                  DiscussionActionMessage(
                    DiscussionAction.Add(
                      Topic.unsafeMake("Shared Discussion Topic"),
                      Person("Client1User"),
                    ),
                  ),
                )
                _ <- actionSent.succeed(())
              yield (),
          )

          // Client 2: authenticates and waits for broadcast
          val client2App = makeCollectingClient(
            client2Messages,
            onHandshake = channel =>
              for
                _ <- channel.send(ticket2)
                _ <- ZIO.withClock(Clock.ClockLive)(ZIO.sleep(100.millis))
                _ <- client2Ready.succeed(())
              yield (),
          )

          // Connect both clients
          ZIO.collectAllPar(List(
            client.socket(client1App),
            client.socket(client2App),
          )).run

          // Wait for synchronization
          actionSent.await.run
          waitForDelivery.run

          val msgs1 = client1Messages.get.run
          val msgs2 = client2Messages.get.run

          val client1Adds = extractAddResults(msgs1)
          val client2Adds = extractAddResults(msgs2)

          assertTrue(
            client1Adds.nonEmpty,
            client2Adds.nonEmpty,
            client1Adds.map(_.topic) == client2Adds.map(_.topic),
          )
      },
    ).provide(
      // Core test infrastructure
      TestClient.layer,
      Scope.default,
      // WebSocket app and routes
      BackendSocketApp.layer,
      TicketRoutesApp.layer,
      // Session management
      SessionService.layer,
      AuthenticatedTicketService.layer,
      ZLayer.succeed(AdminConfig(Set.empty, "test-channel")),
      // Data stores (in-memory for tests)
      ZLayer.make[DiscussionStore](
        DiscussionDataStore.layer(useSampleData = false),
        GlyphiconService.layer,
      ),
      // Supporting services (no-op/minimal for tests)
      TestLayers.lightningTalkServiceLayer,
      TestLayers.hackathonProjectServiceLayer,
      TestLayers.activityServiceLayer,
      TestLayers.slackNotifierLayer,
      TestLayers.confirmedActionRepositoryLayer,
      TestLayers.userRepositoryLayer,
      TestLayers.locationServiceLayer,
      TestLayers.discussionRepositoryLayer,
      TestLayers.schedulingServiceLayer,
      RandomActionSpawner.layer(initialActive = false),
    ) @@ TestAspect.sequential

