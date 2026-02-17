package co.wtf.openspaces

import zio.*
import zio.http.*
import zio.json.*

case class OpenSpacesServerChannel(
  channel: WebSocketChannel):
  def send(
    message: WebSocketMessageFromServer,
  ): ZIO[Any, Throwable, Unit] =
    channel.send(
      ChannelEvent.Read(WebSocketFrame.text(message.toJson)),
    )
