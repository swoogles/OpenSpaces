package co.wtf.openspaces

import zio.*
import zio.http.*
import zio.json.*
import zio.http.ChannelEvent.Read
import zio.http.codec.PathCodec._
import zio.http.codec._
import zio.http.endpoint._
import zio.schema._
import scala.concurrent.Future

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

case class RandomActionClient(
executor: EndpointExecutor[Any, Unit, zio.Scope]
) {

  import zio._
  import zio.http._
  val runtime = Runtime.default
  
  def randomActionToggle: Future[ActiveStatus] = 
    futureDumb(RandomActionApi.randomActionToggle.apply(()))
  
  private def futureDumb[P, I, E, O, A <: AuthType](
  invocation: Invocation[P, Unit, ZNothing, O, zio.http.endpoint.AuthType.None.type]
  ) = {
  
  Unsafe.unsafe { implicit unsafe =>
    println("slightly less dumb future")
    runtime.unsafe.runToFuture(
      ZIO.scoped(
        executor(invocation)
        )
      )
  }
  }
    
  def  randomScheduleActionToggle: Future[ActiveStatus] = {
    futureDumb(RandomActionApi.randomScheduleToggle.apply(()))
  }
}