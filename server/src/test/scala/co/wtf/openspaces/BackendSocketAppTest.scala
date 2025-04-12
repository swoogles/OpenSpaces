package co.wtf.openspaces

import zio.*
import zio.json.*
import zio.test.*
import zio.http.*
import zio.direct.*
import zio.http.ChannelEvent.UserEvent
import java.util.UUID

object BackendSocketAppTest extends ZIOSpecDefault {
  case class IndividualClient(
    frontEndDiscussionState: Ref[DiscussionState],
    socketClient: WebSocketApp[Any]
  )

  object IndividualClient:
    def make(
      slots: List[DaySlots],
      socketClient: (discussionState: Ref[DiscussionState]) => WebSocketApp[Any]
    ): ZIO[Any, Nothing, IndividualClient] =
      defer:
        val frontEndDiscussionState = Ref.make(
          DiscussionState(slots, Map.empty)
        ).run
        
        IndividualClient(frontEndDiscussionState, socketClient(frontEndDiscussionState))

  override def spec = suite("BackendSocketAppTest")(
    test("test") {
      defer:
        val app = ZIO.service[BackendSocketApp].run
        val ticket = ZIO.serviceWithZIO[AuthenticatedTicketService](_.create).debug.run
        val backEndStateOriginal = ZIO.serviceWithZIO[DiscussionDataStore](_.snapshot).run

        TestClient.installSocketApp(app.socketApp).run


        val individualClients = ZIO.foreach(Range(1,2))(_ => IndividualClient.make(
          backEndStateOriginal.slots,
          (discussionState: Ref[DiscussionState]) =>
            Handler.webSocket { channel =>
              channel.receiveAll {
                case ChannelEvent.Read(WebSocketFrame.Text(text)) =>
                  defer:
                    val confirmedAction: DiscussionActionConfirmed = text.fromJson[DiscussionActionConfirmed].getOrElse(???)
                    val state = discussionState.updateAndGet(state => state.apply(confirmedAction)).run

                case ChannelEvent.UserEventTriggered(UserEvent.HandshakeComplete) =>
                  defer:
                    channel.send(ChannelEvent.Read(WebSocketFrame.text(ticket.asInstanceOf[WebSocketMessage].toJson))).run
                    ZIO.unit.run


                case _ =>
                  ZIO.unit
              }
            }
        )
        ).run


        val response = 
            ZIO.foreach(individualClients) { individualClient =>
            ZIO.serviceWithZIO[Client](_.socket(individualClient.socketClient))
                .repeatN(1)
                .debug
                }
                .run

        ZIO.withClock(Clock.ClockLive) {
          ZIO.sleep(500.millis)
        }.run

        ZIO.foreach(individualClients) { individualClient =>
            defer:
                val frontEndState = individualClient.frontEndDiscussionState.get.run
                val backEndState = ZIO.serviceWithZIO[DiscussionDataStore](_.snapshot).run

                assertTrue(
                app.connectedUsers.get.run.size == 1,
                frontEndState == backEndState
                )
        }.run
        assertCompletes
    }
  ).provide(
    BackendSocketApp.layer, 
    TestClient.layer, 
    Scope.default, 
    DiscussionDataStore.layer, 
    AuthenticatedTicketService.layer, 
    GlyphiconService.layer
  )
}
