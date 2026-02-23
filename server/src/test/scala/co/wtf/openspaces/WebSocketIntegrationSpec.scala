package co.wtf.openspaces

import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionActionConfirmed, DiscussionState}
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.lighting_talks.*
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
                  DiscussionActionConfirmed.StateReplace(_),
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
      // Data stores (in-memory for tests)
      ZLayer.make[DiscussionStore](
        DiscussionDataStore.layer(useSampleData = false),
        GlyphiconService.layer,
      ),
      GlyphiconService.layer,
      // Supporting services (no-op/minimal for tests)
      TestLayers.lightningTalkServiceLayer,
      TestLayers.hackathonProjectServiceLayer,
      TestLayers.slackNotifierLayer,
      TestLayers.confirmedActionRepositoryLayer,
      TestLayers.schedulingServiceLayer,
      RandomActionSpawner.layer(initialActive = false),
    ) @@ TestAspect.sequential

/** Test-only layers for services that need database or external dependencies */
object TestLayers:

  /** SchedulingService layer (uses real implementation since it has no DB deps) */
  val schedulingServiceLayer: ZLayer[SessionService & DiscussionStore, Nothing, SchedulingService] =
    SchedulingService.layer

  /** No-op SlackNotifier for tests */
  val slackNotifierLayer: ULayer[co.wtf.openspaces.slack.SlackNotifier] =
    ZLayer.succeed(co.wtf.openspaces.slack.SlackNotifierNoOp())

  /** In-memory LightningTalkService for tests */
  val lightningTalkServiceLayer: ULayer[LightningTalkService] =
    ZLayer.fromZIO:
      for
        stateRef <- Ref.make(LightningTalkState(Map.empty))
      yield LightningTalkService(
        stateRef,
        NoOpLightningTalkRepository,
        NoOpUserRepository,
        NoOpGitHubProfileService,
      )

  /** In-memory HackathonProjectService for tests */
  val hackathonProjectServiceLayer: ULayer[HackathonProjectService] =
    ZLayer.fromZIO:
      for
        stateRef <- Ref.make(HackathonProjectState(Map.empty))
      yield HackathonProjectService(
        stateRef,
        NoOpHackathonProjectRepository,
        NoOpHackathonProjectMemberRepository,
        NoOpUserRepository,
        NoOpGitHubProfileService,
      )

  /** No-op ConfirmedActionRepository for tests */
  val confirmedActionRepositoryLayer
      : ULayer[co.wtf.openspaces.db.ConfirmedActionRepository] =
    ZLayer.succeed(NoOpConfirmedActionRepository)

  // No-op repository implementations

  private object NoOpLightningTalkRepository
      extends co.wtf.openspaces.db.LightningTalkRepository:
    def findById(
      id: Long,
    ): Task[Option[co.wtf.openspaces.db.LightningTalkRow]] = ZIO.none
    def findBySpeaker(
      speaker: String,
    ): Task[Option[co.wtf.openspaces.db.LightningTalkRow]] = ZIO.none
    def findAll: Task[Vector[co.wtf.openspaces.db.LightningTalkRow]] =
      ZIO.succeed(Vector.empty)
    def insert(row: co.wtf.openspaces.db.LightningTalkRow): Task[Unit] =
      ZIO.unit
    def update(row: co.wtf.openspaces.db.LightningTalkRow): Task[Unit] =
      ZIO.unit
    def delete(id: Long): Task[Unit] = ZIO.unit
    def updateSlackThread(
      proposalId: Long,
      channelId: String,
      threadTs: String,
      permalink: String,
    ): Task[Unit] = ZIO.unit

  private object NoOpUserRepository extends co.wtf.openspaces.db.UserRepository:
    def findByUsername(
      username: String,
    ): Task[Option[co.wtf.openspaces.db.UserRow]] = ZIO.none
    def upsert(
      username: String,
      displayName: Option[String],
    ): Task[co.wtf.openspaces.db.UserRow] =
      ZIO.succeed(
        co.wtf.openspaces.db.UserRow(
          username,
          displayName,
          java.time.OffsetDateTime.now(),
        ),
      )
    def findAll: Task[Vector[co.wtf.openspaces.db.UserRow]] =
      ZIO.succeed(Vector.empty)

  private object NoOpGitHubProfileService extends GitHubProfileService:
    def ensureUserWithDisplayName(
      username: String,
    ): Task[co.wtf.openspaces.db.UserRow] =
      ZIO.succeed(
        co.wtf.openspaces.db.UserRow(
          username,
          Some(username),
          java.time.OffsetDateTime.now(),
        ),
      )

  private object NoOpHackathonProjectRepository
      extends co.wtf.openspaces.db.HackathonProjectRepository:
    def findById(
      id: Long,
    ): Task[Option[co.wtf.openspaces.db.HackathonProjectRow]] = ZIO.none
    def findAllActive: Task[Vector[co.wtf.openspaces.db.HackathonProjectRow]] =
      ZIO.succeed(Vector.empty)
    def insert(row: co.wtf.openspaces.db.HackathonProjectRow): Task[Unit] =
      ZIO.unit
    def update(row: co.wtf.openspaces.db.HackathonProjectRow): Task[Unit] =
      ZIO.unit
    def updateOwner(id: Long, newOwner: String): Task[Unit] = ZIO.unit
    def softDelete(id: Long): Task[Unit] = ZIO.unit
    def updateSlackThread(
      id: Long,
      channelId: String,
      threadTs: String,
      permalink: String,
    ): Task[Unit] = ZIO.unit

  private object NoOpHackathonProjectMemberRepository
      extends co.wtf.openspaces.db.HackathonProjectMemberRepository:
    def findActiveByProject(
      projectId: Long,
    ): Task[Vector[co.wtf.openspaces.db.HackathonProjectMemberRow]] =
      ZIO.succeed(Vector.empty)
    def findActiveByUser(
      username: String,
    ): Task[Option[co.wtf.openspaces.db.HackathonProjectMemberRow]] =
      ZIO.none
    def addMember(
      projectId: Long,
      username: String,
    ): Task[co.wtf.openspaces.db.HackathonProjectMemberRow] =
      ZIO.succeed(
        co.wtf.openspaces.db.HackathonProjectMemberRow(
          projectId,
          username,
          java.time.OffsetDateTime.now(),
          None,
        ),
      )
    def removeMember(projectId: Long, username: String): Task[Unit] = ZIO.unit

  private object NoOpConfirmedActionRepository
      extends co.wtf.openspaces.db.ConfirmedActionRepository:
    def append(
      entityType: String,
      actionType: String,
      payload: String,
      actor: Option[String],
    ): Task[co.wtf.openspaces.db.ConfirmedActionRow] =
      ZIO.succeed(
        co.wtf.openspaces.db.ConfirmedActionRow(
          0L,
          java.time.OffsetDateTime.now(),
          entityType,
          actionType,
          payload,
          actor,
        ),
      )
    def findAll: Task[Vector[co.wtf.openspaces.db.ConfirmedActionRow]] =
      ZIO.succeed(Vector.empty)
    def truncate: Task[Unit] = ZIO.unit
