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
import scala.concurrent.ExecutionContext.Implicits.global

case class ActiveStatus(active: Boolean) derives Schema, JsonCodec
case class VersionInfo(version: String) derives Schema, JsonCodec
case class ScheduleResult(scheduled: Int, moved: Int, unscheduled: Int) derives Schema, JsonCodec
case class DeleteTopicsResult(deleted: Int) derives Schema, JsonCodec
case class RefreshStatus(status: String) derives Schema, JsonCodec

// Confirmed action log entry for visualization/replay
case class ConfirmedActionEntry(
  id: Long,
  createdAtEpochMs: Long,
  entityType: String,
  actionType: String,
  payload: String,  // JSON string
  actor: Option[String],  // GitHub username
) derives Schema, JsonCodec

case class ConfirmedActionsResponse(
  actions: List[ConfirmedActionEntry],
) derives Schema, JsonCodec

object RandomActionApi {
  val ticketGet =
    Endpoint(RoutePattern.GET / "ticket")
      .header[String]("Authorization")
      .out[Ticket]

  val refreshGet =
    Endpoint(RoutePattern.GET / "refresh")
      .header(HeaderCodec.cookie.optional)
      .out[RefreshStatus]
      .outHeader(HeaderCodec.setCookie)
      .outHeader(HeaderCodec.setCookie)
      .outHeader(HeaderCodec.setCookie)
      .outHeader(HeaderCodec.setCookie)

  val versionGet =
    Endpoint(RoutePattern.GET / "api" / "version")
      .out[VersionInfo]
  
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

  val deleteAllTopics =
    Endpoint(RoutePattern.POST / "api" / "admin" / "topics" / "delete-all")
      .out[DeleteTopicsResult]

  val runScheduling =
    Endpoint(RoutePattern.POST / "api" / "admin" / "schedule")
      .out[ScheduleResult]

  val hackathonChaosGet =
    Endpoint(RoutePattern.GET / "api" / "admin" / "hackathon-chaos")
      .out[ActiveStatus]

  val hackathonChaosToggle =
    Endpoint(RoutePattern.POST / "api" / "admin" / "hackathon-chaos" / "toggle")
      .out[ActiveStatus]

  val confirmedActionsGet =
    Endpoint(RoutePattern.GET / "api" / "admin" / "confirmed-actions")
      .out[ConfirmedActionsResponse]

  // Documentation-only representation of the primary WebSocket message contract.
  val discussionsWebSocket =
    Endpoint(RoutePattern.GET / "discussions")
      .in[WebSocketMessageFromClient]
      .out[WebSocketMessageFromServer]

  val endpoints =
    List(
      ticketGet,
      refreshGet,
      versionGet,
      randomActionGet,
      randomActionToggle,
      randomScheduleGet,
      randomScheduleToggle,
      hackathonChaosGet,
      hackathonChaosToggle,
      deleteAllTopics,
      runScheduling,
      confirmedActionsGet,
      discussionsWebSocket,
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

  def version: Future[VersionInfo] =
    futureDumb(RandomActionApi.versionGet.apply(()))

  def randomActionStatus: Future[ActiveStatus] =
    futureDumb(RandomActionApi.randomActionGet.apply(()))

  def randomScheduleStatus: Future[ActiveStatus] =
    futureDumb(RandomActionApi.randomScheduleGet.apply(()))
  
  def ticket(accessToken: String): Future[Ticket] =
    futureDumb(
      RandomActionApi.ticketGet.apply(
        s"Bearer $accessToken",
      )
    )

  def refresh: Future[RefreshStatus] =
    futureDumb(RandomActionApi.refreshGet.apply(None)).map(_._1)

  private def futureDumb[P, I, E, O, A <: AuthType](
  invocation: Invocation[P, I, ZNothing, O, zio.http.endpoint.AuthType.None.type]
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

  def deleteAllTopics: Future[DeleteTopicsResult] =
    futureDumb(RandomActionApi.deleteAllTopics.apply(()))

  def runScheduling: Future[ScheduleResult] =
    futureDumb(RandomActionApi.runScheduling.apply(()))

  def hackathonChaosStatus: Future[ActiveStatus] =
    futureDumb(RandomActionApi.hackathonChaosGet.apply(()))

  def hackathonChaosToggle: Future[ActiveStatus] =
    futureDumb(RandomActionApi.hackathonChaosToggle.apply(()))
}
