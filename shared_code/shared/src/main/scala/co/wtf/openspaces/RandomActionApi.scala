package co.wtf.openspaces

import zio.*
import zio.http.*
import zio.json.*
import zio.http.ChannelEvent.Read
import zio.http.codec.PathCodec._
import zio.http.codec._
import zio.http.endpoint._
import zio.schema._

case class ActiveStatus(active: Boolean) derives Schema, JsonCodec

object RandomActionApi {
  
  val randomActionGet =
    Endpoint(RoutePattern.GET / "api" / "admin" / "random-actions")
    //  PathCodec.int("id")
    .out[ActiveStatus]
  
  val randomActionToggle =
    Endpoint(RoutePattern.POST / "api" / "admin" / "random-actions" / "toggle")
      .in[Unit]
      .out[ActiveStatus]
  
  val randomScheduleGet =
    Endpoint(RoutePattern.GET / "api" / "admin" / "schedule-chaos")
    .out[ActiveStatus]
  
  val randomScheduleToggle =
    Endpoint(RoutePattern.POST / "api" / "admin" / "schedule-chaos" / "toggle")
      .out[ActiveStatus]
  
  val endpoints =
    List(
      randomActionGet,
      randomActionToggle,
      randomScheduleGet,
      randomScheduleToggle,
    )
}