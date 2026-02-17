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

/* 
Best-practice policy for your requirement

- “If online, always latest app, if fingerprint is different. Don't redownload the identical app.”:
    - HTML/network shell: network-first no-store
    - API/auth: network-only
    - static JS/CSS: only cache if fingerprinted
    - media/images: stale-while-revalidate is fine
*/
object ServiceWorker:
  private val cacheName = "sticky-icky-cache"

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
      (event: ExtendableEvent) =>
        // Take control immediately so updates apply without reload
        self.clients.claim(),
    )

    self.addEventListener(
      "fetch",
      (event: FetchEvent) => {
        val request = event.request
        if request.method.toString != "GET" then
          event.respondWith(fetch(request).toFuture.toJSPromise)
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


