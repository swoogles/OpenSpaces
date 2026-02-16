package co.wtf.openspaces.services

import com.raquo.laminar.api.L.EventStream
import org.scalajs.dom
import org.scalajs.dom.window
import scala.scalajs.js.URIUtils
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import co.wtf.openspaces.{Person, RandomActionClient}

object AuthService:
  private var refreshInFlight: Option[Future[Boolean]] = None

  def getCookie(
    name: String,
  ): Option[String] = {
    val cookieString = dom.document.cookie
    val cookies = cookieString.split(";")

    cookies.find(_.trim.startsWith(s"$name=")) match {
      case Some(cookie) =>
        val encodedValue = cookie.trim.substring(name.length + 1)
        Some(URIUtils.decodeURIComponent(encodedValue))
      case None => None
    }
  }

  def deleteCookie(
    name: String,
  ) =
    dom.document.cookie =
      name + "=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;";

  /** Check if the access token is expired or about to expire (within 5 minutes) */
  def isAccessTokenExpired: Boolean = {
    val accessToken = getCookie("access_token")
    if (accessToken.isEmpty) {
      true
    } else {
      getCookie("access_token_expires_at") match {
        case Some(expiresAt) =>
          val expiryTime = expiresAt.toLongOption.getOrElse(0L)
          val now = (System.currentTimeMillis() / 1000)
          val bufferSeconds = 300 // 5 minute buffer
          now >= (expiryTime - bufferSeconds)
        case None =>
          // No expiry cookie means old session - treat as expired to trigger refresh
          true
      }
    }
  }

  /** Refresh the access token using the refresh token */
  def refreshAccessToken(
    randomActionClient: RandomActionClient,
  ): Future[Boolean] =
    randomActionClient.refresh.map(_.status == "refreshed").recover { case _ => false }

  /** Ensure the access token is still fresh, refreshing when close to expiry.
    * Uses a single in-flight refresh to avoid duplicate /refresh calls.
    */
  def ensureFreshAccessToken(
    randomActionClient: RandomActionClient,
  ): Future[Boolean] =
    if !isAccessTokenExpired then
      Future.successful(true)
    else
      refreshInFlight match
        case Some(existingRefresh) =>
          existingRefresh
        case None =>
          val refreshFuture = refreshAccessToken(randomActionClient).andThen { case _ =>
            refreshInFlight = None
          }
          refreshInFlight = Some(refreshFuture)
          refreshFuture

  /** Gets the GitHub username from the cookie set during OAuth login. The name
    * is immutable and comes directly from GitHub.
    */
  def getGitHubUsername(): com.raquo.laminar.api.L.Var[Person] =
    val username = getCookie("github_username")
      .map(Person(_))
      .getOrElse(Person(""))
    com.raquo.laminar.api.L.Var(username)

  /** Fetch a ticket, automatically refreshing the access token if needed */
  def fetchTicketWithRefresh(randomActionClient: RandomActionClient): EventStream[String] = {
    EventStream.fromFuture(fetchTicketAsync(randomActionClient))
  }

  /** Fetch a fresh auth ticket, refreshing access token if needed.
    * Returns a Future that resolves to the ticket response text.
    */
  def fetchTicketAsync(randomActionClient: RandomActionClient): Future[String] =
    ensureFreshAccessToken(randomActionClient).flatMap { refreshed =>
      if refreshed then
        randomActionClient
          .ticket(getCookie("access_token").getOrElse(""))
      else
        window.location.href = "/auth"
        Future.failed(new Exception("Token refresh failed"))
    }
