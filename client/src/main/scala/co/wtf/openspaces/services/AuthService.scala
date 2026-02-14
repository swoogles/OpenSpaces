package co.wtf.openspaces.services

import com.raquo.laminar.api.L.{EventStream, FetchStream}
import org.scalajs.dom
import org.scalajs.dom.window
import scala.scalajs.js.URIUtils
import scala.concurrent.ExecutionContext.Implicits.global

import co.wtf.openspaces.{Person, Ticket}

object AuthService:
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
  def refreshAccessToken(): scala.concurrent.Future[Boolean] = {
    import scala.scalajs.js
    import scala.concurrent.ExecutionContext.Implicits.global

    val fetchPromise = dom.fetch("/refresh", new dom.RequestInit {
      method = dom.HttpMethod.GET
    })

    fetchPromise.toFuture.map { response =>
      if (response.ok) {
        // Cookies are automatically updated by the response
        true
      } else {
        // Refresh failed - will redirect to login
        false
      }
    }.recover { case _ => false }
  }

  /** Gets the GitHub username from the cookie set during OAuth login. The name
    * is immutable and comes directly from GitHub.
    */
  def getGitHubUsername(): com.raquo.laminar.api.L.Var[Person] =
    val username = getCookie("github_username")
      .map(Person(_))
      .getOrElse(Person(""))
    com.raquo.laminar.api.L.Var(username)

  /** Fetch a ticket, automatically refreshing the access token if needed */
  def fetchTicketWithRefresh(): EventStream[String] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    // Check if token needs refresh before making the request
    if (isAccessTokenExpired) {
      EventStream.fromFuture(
        refreshAccessToken().flatMap { refreshed =>
          if (refreshed) {
            // Token refreshed, now fetch the ticket
            dom.fetch("/ticket", new dom.RequestInit {
              method = dom.HttpMethod.GET
              headers = new dom.Headers(scalajs.js.Array(
                scalajs.js.Array("Authorization", s"Bearer ${getCookie("access_token").getOrElse("")}")
              ))
            }).toFuture.flatMap(_.text().toFuture)
          } else {
            // Refresh failed, redirect to login
            window.location.href = "/auth"
            scala.concurrent.Future.failed(new Exception("Token refresh failed"))
          }
        }
      )
    } else {
      // Token is still valid, fetch directly
      FetchStream.get(
        "/ticket",
        fetchOptions =>
          fetchOptions.headers(
            "Authorization" -> s"Bearer ${getCookie("access_token").get}",
          ),
      )
    }
  }

  /** Fetch a fresh auth ticket, refreshing access token if needed.
    * Returns a Future that resolves to the ticket response text.
    */
  def fetchTicketAsync(): scala.concurrent.Future[String] =
    if isAccessTokenExpired then
      println("Access token expired, refreshing...")
      refreshAccessToken().flatMap { refreshed =>
        if refreshed then
          dom.fetch("/ticket", new dom.RequestInit {
            method = dom.HttpMethod.GET
            headers = new dom.Headers(scalajs.js.Array(
              scalajs.js.Array("Authorization", s"Bearer ${getCookie("access_token").getOrElse("")}")
            ))
          }).toFuture.flatMap { response =>
            if response.ok then response.text().toFuture
            else if response.status == 401 then
              scala.concurrent.Future.failed(new Exception("Unauthorized - please log in again"))
            else
              scala.concurrent.Future.failed(new Exception(s"HTTP ${response.status}"))
          }
        else
          scala.concurrent.Future.failed(new Exception("Token refresh failed"))
      }
    else
      dom.fetch("/ticket", new dom.RequestInit {
        method = dom.HttpMethod.GET
        headers = new dom.Headers(scalajs.js.Array(
          scalajs.js.Array("Authorization", s"Bearer ${getCookie("access_token").getOrElse("")}")
        ))
      }).toFuture.flatMap { response =>
        if response.ok then response.text().toFuture
        else if response.status == 401 then
          scala.concurrent.Future.failed(new Exception("Unauthorized - please log in again"))
        else
          scala.concurrent.Future.failed(new Exception(s"HTTP ${response.status}"))
      }
