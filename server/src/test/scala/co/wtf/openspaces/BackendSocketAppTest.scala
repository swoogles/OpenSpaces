package co.wtf.openspaces

import zio.*
import zio.json.*
import zio.test.*
import zio.http.*
import zio.direct.*
import zio.http.ChannelEvent.UserEvent
import java.util.UUID
import zio.http.ChannelEvent.ExceptionCaught
import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEventTriggered
import zio.http.ChannelEvent.Registered
import zio.http.ChannelEvent.Unregistered
import java.time.LocalDateTime
import zio.schema.codec.JsonCodec.zioJsonBinaryCodec
// TODO Use map/contrmap on
// type WebSocketChannel = Channel[WebSocketChannelEvent, WebSocketChannelEvent]
//
// And Figure out what the right output types are.

/*
                        channel.map {
                          case Read(message) =>
                            Read(
                              message
                                .fromJson[DiscussionActionConfirmed]
                                .getOrElse(???)
                            )
                          case other => other
                        }
 */

case class OpenSpacesClientChannel(
  channel: WebSocketChannel):
  def send(
    message: WebSocketMessage,
  ): ZIO[Any, Throwable, Unit] =
    channel.send(
      ChannelEvent.Read(WebSocketFrame.text(message.toJson)),
    )

object BackendSocketAppTest extends ZIOSpecDefault {
  case class IndividualClient(
    frontEndDiscussionState: Ref[DiscussionState],
    socketClient: WebSocketApp[Any])

  object IndividualClient:
    def make(
      slots: List[DaySlots],
      socketClient: (
        discussionState: Ref[DiscussionState],
      ) => WebSocketApp[Any],
    ): ZIO[Any, Nothing, IndividualClient] =
      defer:
        val frontEndDiscussionState = Ref
          .make(
            DiscussionState(slots, Map.empty),
          )
          .run

        IndividualClient(frontEndDiscussionState,
                         socketClient(frontEndDiscussionState),
        )

  private def waitForStateSync: ZIO[Any, Nothing, Unit] =
    ZIO.withClock(Clock.ClockLive) {
      ZIO.sleep(500.millis)
    }

  override def spec =
    suite("BackendSocketAppTest")(
      test("test") {
        defer:
          TestRandom
            .feedUUIDs(
              UUID.fromString("a2c8ccb8-191a-4233-9b34-3e3111a4adaa"),
              UUID.fromString("b2c8ccb8-191a-4233-9b34-3e3111a4adab"),
            )
            .run
          val app = ZIO.service[BackendSocketApp].run
          val backEndStateOriginal =
            ZIO.serviceWithZIO[DiscussionDataStore](_.snapshot).run

          TestClient.installSocketApp(app.socketApp).run

          val ticketRoutesApp = ZIO.service[TicketRoutesApp].run
          TestClient.addRoutes(ticketRoutesApp.routes).run

          val client = ZIO.service[Client].run

          val individualClients = ZIO
            .foreach(Range(0, 2)) { _ =>
              defer:
                val ticketResponse =
                  client(
                    Request
                      .get(URL.root / "ticket")
                      .addHeader(
                        Header.Authorization.Bearer("some junk token"),
                      ),
                  ).run

                val ticket =
                  ticketResponse.body.to[Ticket].run

                IndividualClient
                  .make(
                    backEndStateOriginal.slots,
                    (discussionState: Ref[DiscussionState]) =>
                      Handler.webSocket { channel =>
                        val openSpacesClientChannel =
                          OpenSpacesClientChannel(channel)
                        channel.receiveAll {
                          case ChannelEvent
                                .Read(WebSocketFrame.Text(text)) =>
                            defer:
                              val confirmedAction = text
                                .fromJson[DiscussionActionConfirmed]
                                .getOrElse(???)
                              val state = discussionState
                                .updateAndGet(state =>
                                  state.apply(confirmedAction),
                                )
                                .run

                          case ChannelEvent.UserEventTriggered(
                                UserEvent.HandshakeComplete,
                              ) =>
                            defer:
                              openSpacesClientChannel
                                .send(ticket)
                                .run

                              openSpacesClientChannel
                                .send(
                                  DiscussionAction.Delete(
                                    TopicId(1),
                                  ),
                                )
                                .run
                        }
                      },
                  )
                  .run
            }
            .run

          ZIO
            .foreach(individualClients) { individualClient =>
              ZIO
                .serviceWithZIO[Client](
                  _.socket(individualClient.socketClient),
                )
            }
            .run

          waitForStateSync.run

          ZIO
            .foreach(individualClients) { individualClient =>
              defer:
                val frontEndState =
                  individualClient.frontEndDiscussionState.get.run
                val backEndState = ZIO
                  .serviceWithZIO[DiscussionDataStore](_.snapshot)
                  .run

                assertTrue(
                  app.discussionService.connectedUsers.get.run.size == 1,
                  frontEndState == backEndState,
                )
            }
            .run
          assertCompletes
      },
      test("add new discussion") {
        defer:
          TestRandom
            .feedUUIDs(
              UUID.fromString("a2c8ccb8-191a-4233-9b34-3e3111a4adaa"),
            )
            .run
          val app = ZIO.service[BackendSocketApp].run
          val backEndStateOriginal =
            ZIO.serviceWithZIO[DiscussionDataStore](_.snapshot).run

          TestClient.installSocketApp(app.socketApp).run

          val ticketRoutesApp = ZIO.service[TicketRoutesApp].run
          TestClient.addRoutes(ticketRoutesApp.routes).run

          val client = ZIO.service[Client].run

          val individualClient = IndividualClient
            .make(
              backEndStateOriginal.slots,
              (discussionState: Ref[DiscussionState]) =>
                Handler.webSocket { channel =>
                  val openSpacesClientChannel =
                    OpenSpacesClientChannel(channel)
                  channel.receiveAll {
                    case ChannelEvent.Read(
                          WebSocketFrame.Text(text),
                        ) =>
                      defer:
                        val confirmedAction = text
                          .fromJson[DiscussionActionConfirmed]
                          .getOrElse(???)
                        val state = discussionState
                          .updateAndGet(state =>
                            state.apply(confirmedAction),
                          )
                          .run

                    case ChannelEvent.UserEventTriggered(
                          UserEvent.HandshakeComplete,
                        ) =>
                      defer:
                        val ticketResponse = client
                          .batched(
                            Request
                              .get(URL.root / "ticket")
                              .addHeader(
                                Header.Authorization
                                  .Bearer("some junk token"),
                              ),
                          )
                          .run

                        val ticket =
                          ticketResponse.body.to[Ticket].run

                        openSpacesClientChannel.send(ticket).run

                        // Add a new discussion
                        openSpacesClientChannel
                          .send(
                            DiscussionAction.Add(
                              Topic.parseOrDie("Test Discussion"),
                              Person("Test User"),
                            ),
                          )
                          .run
                  }
                },
            )
            .run

          ZIO
            .serviceWithZIO[Client](
              _.socket(individualClient.socketClient),
            )
            .run

          waitForStateSync.run

          defer:
            val frontEndState =
              individualClient.frontEndDiscussionState.get.run
            val backEndState = ZIO
              .serviceWithZIO[DiscussionDataStore](_.snapshot)
              .run

            assertTrue(
              frontEndState == backEndState,
              frontEndState.data.size == 1,
              frontEndState.data.values.head.topic.unwrap == "Test Discussion",
              frontEndState.data.values.head.facilitator.unwrap == "Test User",
            )
          .run
          assertCompletes
      },
      test("vote on discussion") {
        defer:
          TestRandom
            .feedUUIDs(
              UUID.fromString("a2c8ccb8-191a-4233-9b34-3e3111a4adaa"),
            )
            .run
          val app = ZIO.service[BackendSocketApp].run
          val backEndStateOriginal =
            ZIO.serviceWithZIO[DiscussionDataStore](_.snapshot).run

          TestClient.installSocketApp(app.socketApp).run

          val ticketRoutesApp = ZIO.service[TicketRoutesApp].run
          TestClient.addRoutes(ticketRoutesApp.routes).run

          val client = ZIO.service[Client].run

          val individualClient = IndividualClient
            .make(
              backEndStateOriginal.slots,
              (discussionState: Ref[DiscussionState]) =>
                Handler.webSocket { channel =>
                  val openSpacesClientChannel =
                    OpenSpacesClientChannel(channel)
                  channel.receiveAll {
                    case ChannelEvent.Read(
                          WebSocketFrame.Text(text),
                        ) =>
                      defer:
                        val confirmedAction = text
                          .fromJson[DiscussionActionConfirmed]
                          .getOrElse(???)
                        val state = discussionState
                          .updateAndGet(state =>
                            state.apply(confirmedAction),
                          )
                          .run

                    case ChannelEvent.UserEventTriggered(
                          UserEvent.HandshakeComplete,
                        ) =>
                      defer:
                        val ticketResponse = client
                          .batched(
                            Request
                              .get(URL.root / "ticket")
                              .addHeader(
                                Header.Authorization
                                  .Bearer("some junk token"),
                              ),
                          )
                          .run

                        val ticket =
                          ticketResponse.body.to[Ticket].run

                        openSpacesClientChannel.send(ticket).run

                        openSpacesClientChannel
                          .send(
                            DiscussionAction.Add(
                              Topic.parseOrDie("Test Discussion"),
                              Person("Test User"),
                            ),
                          )
                          .run

                        openSpacesClientChannel
                          .send(
                            DiscussionAction.Vote(
                              TopicId(1),
                              Feedback(Person("Voter"),
                                       VotePosition.Interested,
                              ),
                            ),
                          )
                          .run
                  }
                },
            )
            .run

          ZIO
            .serviceWithZIO[Client](
              _.socket(individualClient.socketClient),
            )
            .run

          waitForStateSync.run

          defer:
            val frontEndState =
              individualClient.frontEndDiscussionState.get.run
            val backEndState = ZIO
              .serviceWithZIO[DiscussionDataStore](_.snapshot)
              .run

            val data = frontEndState.data.values
            assertTrue(
              // frontEndState == backEndState,
              data.size == 1,
              // data.head.interestedParties.size == 2, // Facilitator + voter
              // data.head.interestedParties.exists(_.voter.unwrap == "Voter")
            )
          .run
      },
      test("schedule and unschedule discussion") {
        defer:
          TestRandom
            .feedUUIDs(
              UUID.fromString("a2c8ccb8-191a-4233-9b34-3e3111a4adaa"),
            )
            .run
          val app = ZIO.service[BackendSocketApp].run
          val backEndStateOriginal =
            ZIO.serviceWithZIO[DiscussionDataStore](_.snapshot).run

          TestClient.installSocketApp(app.socketApp).run

          val ticketRoutesApp = ZIO.service[TicketRoutesApp].run
          TestClient.addRoutes(ticketRoutesApp.routes).run

          val client = ZIO.service[Client].run

          val individualClient = IndividualClient
            .make(
              backEndStateOriginal.slots,
              (discussionState: Ref[DiscussionState]) =>
                Handler.webSocket { channel =>
                  val openSpacesClientChannel =
                    OpenSpacesClientChannel(channel)
                  channel.receiveAll {
                    case ChannelEvent.Read(
                          WebSocketFrame.Text(text),
                        ) =>
                      defer:
                        val confirmedAction = text
                          .fromJson[DiscussionActionConfirmed]
                          .getOrElse(???)
                        val state = discussionState
                          .updateAndGet(state =>
                            state.apply(confirmedAction),
                          )
                          .run

                    case ChannelEvent.UserEventTriggered(
                          UserEvent.HandshakeComplete,
                        ) =>
                      defer:
                        val ticketResponse = client
                          .batched(
                            Request
                              .get(URL.root / "ticket")
                              .addHeader(
                                Header.Authorization
                                  .Bearer("some junk token"),
                              ),
                          )
                          .run

                        val ticket =
                          ticketResponse.body.to[Ticket].run

                        openSpacesClientChannel.send(ticket).run

                        // Add a new discussion
                        openSpacesClientChannel
                          .send(
                            DiscussionAction.Add(
                              Topic.parseOrDie("Test Discussion"),
                              Person("Test User"),
                            ),
                          )
                          .run

                        // Schedule the discussion
                        val roomSlot = RoomSlot(
                          Room.king,
                          TimeSlot(
                            "8:00-8:50",
                            LocalDateTime.parse(
                              "2025-06-24T08:00:00",
                            ),
                            LocalDateTime.parse("2025-06-24T08:50:00"),
                          ),
                        )
                        openSpacesClientChannel
                          .send(
                            DiscussionAction.UpdateRoomSlot(
                              TopicId(1),
                              roomSlot,
                            ),
                          )
                          .run

                        // Unschedule the discussion
                        openSpacesClientChannel
                          .send(
                            DiscussionAction.Unschedule(TopicId(1)),
                          )
                          .run
                  }
                },
            )
            .run

          client.socket(individualClient.socketClient).run

          waitForStateSync.run

          defer:
            val frontEndState =
              individualClient.frontEndDiscussionState.get.run
            val backEndState = ZIO
              .serviceWithZIO[DiscussionDataStore](_.snapshot)
              .run

            assertTrue(
              frontEndState == backEndState,
              frontEndState.data.size == 1,
              frontEndState.data.values.head.roomSlot.isEmpty, // Should be unscheduled
            )
          .run
          assertCompletes
      },
      test("unticketed action rejection") {
        defer:
          val app = ZIO.service[BackendSocketApp].run
          val backEndStateOriginal =
            ZIO.serviceWithZIO[DiscussionDataStore](_.snapshot).run

          TestClient.installSocketApp(app.socketApp).run

          val individualClient = IndividualClient
            .make(
              backEndStateOriginal.slots,
              (discussionState: Ref[DiscussionState]) =>
                Handler.webSocket { channel =>
                  val openSpacesClientChannel =
                    OpenSpacesClientChannel(channel)
                  channel.receiveAll {
                    case ChannelEvent.Read(
                          WebSocketFrame.Text(text),
                        ) =>
                      defer:
                        val confirmedAction = text
                          .fromJson[DiscussionActionConfirmed]
                          .getOrElse(???)
                        val state = discussionState
                          .updateAndGet(state =>
                            state.apply(confirmedAction),
                          )
                          .run

                    case ChannelEvent.UserEventTriggered(
                          UserEvent.HandshakeComplete,
                        ) =>
                      defer:
                        // Try to add a discussion without a ticket
                        openSpacesClientChannel
                          .send(
                            DiscussionAction.Add(
                              Topic.parseOrDie("Test Discussion"),
                              Person("Test User"),
                            ),
                          )
                          .run
                  }
                },
            )
            .run

          ZIO
            .serviceWithZIO[Client](
              _.socket(individualClient.socketClient),
            )
            .run

          waitForStateSync.run

          defer:
            val frontEndState =
              individualClient.frontEndDiscussionState.get.run
            val backEndState = ZIO
              .serviceWithZIO[DiscussionDataStore](_.snapshot)
              .run

            assertTrue(
              frontEndState == backEndState,
              frontEndState.data.isEmpty, // No discussions should be added without a ticket
            )
          .run
          assertCompletes
      },
    ).provide(
      BackendSocketApp.layer,
      TestClient.layer,
      Scope.default,
      DiscussionDataStore.layer,
      AuthenticatedTicketService.layer,
      TicketRoutesApp.layer,
      GlyphiconService.layer,
      DiscussionService.layer,
    )
}
