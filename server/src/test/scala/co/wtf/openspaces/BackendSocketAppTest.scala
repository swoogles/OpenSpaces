package co.wtf.openspaces

import zio.*
import zio.test.*
import zio.http.*
import zio.direct.*

object BackendSocketAppTest extends ZIOSpecDefault {
  override def spec = suite("BackendSocketAppTest")(
    test("test") {
        defer:
            val app = ZIO.service[BackendSocketApp].run
            TestClient.installSocketApp(app.socketApp).run
            val socketClient: WebSocketApp[Any] =
                Handler.webSocket { channel =>
                channel.receiveAll {
                    case ChannelEvent.Read(WebSocketFrame.Text("Hi Client")) =>
                    channel.send(ChannelEvent.Read(WebSocketFrame.text("Hi Server")))

                    case _ =>
                    ZIO.unit
                }
                }
            val response = 
                ZIO.serviceWithZIO[Client](_.socket(socketClient))
                .debug
                .run
            assertTrue(true)
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
