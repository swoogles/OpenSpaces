package co.wtf.openspaces.pwa

import org.scalajs.dom.experimental.Fetch._
import org.scalajs.dom.ServiceWorkerGlobalScope
import org.scalajs.dom.ServiceWorkerGlobalScope.self
import org.scalajs.dom.experimental.serviceworkers.{ExtendableEvent, FetchEvent}
import org.scalajs.dom.experimental._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object ServiceWorker:
  private val cacheName = "sticky-icky-cache-v2"

  private val assetsToCache: js.Array[RequestInfo] =
    List[RequestInfo](
    //   "/",
      "/index.html",
    //   "/client-fastopt.js",
    //   "/styles.css",
      "/manifest.webmanifest",
    //   "/public/images/BILLDING_LogoMark-256.png",
    //   "/public/images/BILLDING_LogoMark-500.png",
    ).toJSArray

  def main(args: Array[String]): Unit =
    self.addEventListener(
      "install",
      (event: ExtendableEvent) => {
        self.skipWaiting()
        event.waitUntil(preCache().toJSPromise)
      },
    )

    self.addEventListener(
      "activate",
      (event: ExtendableEvent) => {
        event.waitUntil(
          clearOldCaches()
            .map(_ => self.clients.claim())
            .toJSPromise,
        )
      },
    )

    self.addEventListener(
      "fetch",
      (event: FetchEvent) => {
        val request = event.request
        if request.method.toString != "GET" then
          event.respondWith(fetch(request).toFuture.toJSPromise)
        else if bypassCache(request) then
          event.respondWith(fetch(request).toFuture.toJSPromise)
        else if isAppShellRequest(request) then
          event.respondWith(networkFirst(request).toJSPromise)
        else
          event.respondWith(
            staleWhileRevalidate(event).toJSPromise,
          )
      },
    )

  private def preCache(): Future[Unit] =
    if assetsToCache.isEmpty then Future.unit
    else
      self.caches
        .flatMap(
          _.open(cacheName).toFuture
            .flatMap(_.addAll(assetsToCache).toFuture),
        )
        .getOrElse(Future.unit)

  private def clearOldCaches(): Future[Unit] =
    self.caches
      .map(
        _.keys().toFuture.flatMap { cacheNames =>
          val removals = cacheNames.toSeq
            .filter(_ != cacheName)
            .map(name =>
              self.caches
                .map(_.delete(name).toFuture.map(_ => ()))
                .getOrElse(Future.unit),
            )
          Future.sequence(removals).map(_ => ())
        },
      )
      .getOrElse(Future.unit)

  private def bypassCache(request: Request): Boolean =
    val path = new URL(request.url).pathname
    path.startsWith("/api/") || path == "/discussions"

  private def isAppShellRequest(request: Request): Boolean =
    val path = new URL(request.url).pathname
    request.mode.toString == "navigate" ||
      path == "/" ||
      path == "/index.html" ||
      path.endsWith(".js") ||
      path.endsWith(".css") ||
      path.endsWith(".webmanifest")

  private def networkFirst(request: Request): Future[Response] =
    self.caches
      .map(
        _.open(cacheName).toFuture.flatMap { cache =>
          fetch(request).toFuture
            .flatMap { networkResp =>
              if networkResp != null && networkResp.ok then
                cache
                  .put(request, networkResp.clone())
                  .toFuture
                  .map(_ => networkResp)
              else
                Future.successful(networkResp)
            }
            .recoverWith { case _ =>
              cache
                .`match`(request)
                .toFuture
                .flatMap {
                  case cached: Response => Future.successful(cached)
                  case _                => fetch(request).toFuture
                }
            }
        },
      )
      .getOrElse(fetch(request).toFuture)

  private def staleWhileRevalidate(
    event: FetchEvent,
  ): Future[Response] = {
    val request = event.request
    self.caches
      .map(
        _.open(cacheName).toFuture.flatMap { cache =>
          cache
            .`match`(request)
            .toFuture
            .flatMap {
              case cached: Response =>
                val updateF =
                  fetch(request).toFuture
                    .flatMap { networkResp =>
                      if networkResp != null && networkResp.ok then
                        cache
                          .put(request, networkResp.clone())
                          .toFuture
                          .map(_ => ())
                      else Future.unit
                    }
                    .recover { case _ => () }
                event.waitUntil(updateF.toJSPromise)
                Future.successful(cached)
              case _ =>
                fetch(request).toFuture.flatMap { networkResp =>
                  if networkResp != null && networkResp.ok then
                    cache
                      .put(request, networkResp.clone())
                      .toFuture
                      .map(_ => networkResp)
                  else Future.successful(networkResp)
                }
            }
        },
      )
      .getOrElse(fetch(request).toFuture)
  }

