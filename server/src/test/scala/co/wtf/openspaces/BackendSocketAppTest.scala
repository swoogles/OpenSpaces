package co.wtf.openspaces

import zio.*
import zio.json.*
import zio.test.*
import zio.http.*
import zio.direct.*
import zio.http.ChannelEvent.UserEvent
import java.util.UUID

object BackendSocketAppTest extends ZIOSpecDefault {
  override def spec = suite("BackendSocketAppTest")(
    test("test") {
        defer:
            val app = ZIO.service[BackendSocketApp].run
            val ticket = ZIO.serviceWithZIO[AuthenticatedTicketService](_.create).debug.run
            val backEndStateOriginal = ZIO.serviceWithZIO[DiscussionDataStore](_.snapshot).run


            val frontEndDiscussionState = Ref.make(
                DiscussionState(backEndStateOriginal.slots, Map.empty)
            ).run

            TestClient.installSocketApp(app.socketApp).run

            val promise = Promise.make[Nothing, Unit].run

            val socketClient: WebSocketApp[Any] =
                Handler.webSocket { channel =>
                channel.receiveAll {
                    case ChannelEvent.Read(WebSocketFrame.Text(text)) =>
                        defer:
                            val confirmedAction: DiscussionActionConfirmed = text.fromJson[DiscussionActionConfirmed].getOrElse(???)
                            val state = frontEndDiscussionState.updateAndGet(state => state.apply(confirmedAction)).run
                            // channel.send(ChannelEvent.Read(WebSocketFrame.text("Hi Server"))).run

                    case ChannelEvent.UserEventTriggered(UserEvent.HandshakeComplete) =>
                        defer:
                            channel.send(ChannelEvent.Read(WebSocketFrame.text(ticket.asInstanceOf[WebSocketMessage].toJson))).run
                            ZIO.unit.run
                    case _ =>
                        ZIO.unit
                }
                }
            val response = 
                ZIO.serviceWithZIO[Client](_.socket(socketClient))
                .debug
                .run
                // TODO Do we need a delay here to make it consisten? That feels very unlikely. 
                // Maybe I need to set a Promise at the end of the Socket Interaction, and wait on that?
                ZIO.withClock(Clock.ClockLive) {
                    ZIO.sleep(100.millis)
                }.run
            val frontEndState = frontEndDiscussionState.get.run
            val backEndState = ZIO.serviceWithZIO[DiscussionDataStore](_.snapshot).run
            assertTrue(
                app.connectedUsers.get.run.size == 1,
                frontEndState == backEndState,
            )
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
