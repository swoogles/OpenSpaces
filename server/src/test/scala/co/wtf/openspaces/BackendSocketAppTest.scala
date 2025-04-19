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
  channel: WebSocketChannel,
):
  def send(message: WebSocketMessage): ZIO[Any, Throwable, Unit] =
    channel.send(ChannelEvent.Read(WebSocketFrame.text(message.toJson)))

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

          println("Original size: " + backEndStateOriginal.data.size)

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
                  ticketResponse.body.asString.run
                    .fromJson[Ticket]
                    .getOrElse(???)

                IndividualClient
                  .make(
                    backEndStateOriginal.slots,
                    (discussionState: Ref[DiscussionState]) =>
                      Handler.webSocket { channel =>
                        val openSpacesClientChannel = OpenSpacesClientChannel(channel)
                        channel.receiveAll {
                          case ChannelEvent
                                .Read(WebSocketFrame.Text(text)) =>
                            defer:
                              val confirmedAction
                                : DiscussionActionConfirmed = text
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
                                    TopicId(1)
                                  )
                                )
                              .run
                        }
                      },
                  )
                  .run
            }
            .run

          val response =
            ZIO
              .foreach(individualClients) { individualClient =>
                ZIO
                  .serviceWithZIO[Client](
                    _.socket(individualClient.socketClient),
                  )
              }
              .run

          ZIO
            .withClock(Clock.ClockLive) {
              ZIO.sleep(500.millis)
            }
            .run

          ZIO
            .foreach(individualClients) { individualClient =>
              defer:
                val frontEndState =
                  individualClient.frontEndDiscussionState.get.run
                val backEndState = ZIO
                  .serviceWithZIO[DiscussionDataStore](_.snapshot)
                  .run

                println("Final size: " + backEndState.data.size)

                assertTrue(
                  app.connectedUsers.get.run.size == 1,
                  frontEndState == backEndState,
                )
            }
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
    )
}
