package co.wtf.openspaces.pwa

import org.scalajs.dom
import org.scalajs.dom.ServiceWorkerGlobalScope.self
import org.scalajs.dom.experimental.Fetch._
import org.scalajs.dom.experimental._
import org.scalajs.dom.experimental.serviceworkers.{ExtendableEvent, FetchEvent}

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
  private val cacheVersion = "v2"
  private val staticCacheName = s"sticky-icky-static-$cacheVersion"
  private val mediaCacheName = s"sticky-icky-media-$cacheVersion"
  private val cachePrefixes = List("sticky-icky-static-", "sticky-icky-media-")

  def main(args: Array[String]): Unit =
    self.addEventListener(
      "install",
      (_: ExtendableEvent) => {
        self.skipWaiting()
      },
    )

    self.addEventListener(
      "activate",
      (event: ExtendableEvent) =>
        event.waitUntil(
          cleanupOldCaches()
            .flatMap(_ => self.clients.claim().toFuture.map(_ => ()))
            .toJSPromise,
        ),
    )

    self.addEventListener(
      "message",
      (event: dom.MessageEvent) => {
        if event.data == "SKIP_WAITING" then self.skipWaiting()
      },
    )

    self.addEventListener(
      "fetch",
      (event: FetchEvent) => {
        val request = event.request
        val responseF =
          if request.method.toString != "GET" then
            fetch(request).toFuture
          else
            handleGet(event)

        event.respondWith(responseF.recoverWith { case _ => fetch(request).toFuture }.toJSPromise)
      },
    )

  private def handleGet(event: FetchEvent): Future[Response] =
    val request = event.request
    val url = new dom.URL(request.url)
    val path = url.pathname
    val search = url.search
    val mode = request.mode.toString

    if isApiOrAuthPath(path) then
      // Always network for auth/API routes.
      fetch(request).toFuture
    else if mode == "navigate" || path == "/" || path == "/index.html" then
      // HTML shell: always network when available.
      fetch(request).toFuture
    else if isFingerprintedStatic(path, search) then
      cacheFirst(staticCacheName, request)
    else if isMedia(request, path) then
      staleWhileRevalidate(mediaCacheName, event, request)
    else
      // For non-fingerprinted assets, avoid cache pinning stale versions.
      fetch(request).toFuture

  private def isApiOrAuthPath(path: String): Boolean =
    path == "/ticket" ||
      path == "/refresh" ||
      path.startsWith("/api/") ||
      path == "/auth" ||
      path.startsWith("/callback")

  private def isFingerprintedStatic(path: String, search: String): Boolean =
    (path.endsWith(".js") || path.endsWith(".css")) && search.contains("v=")

  private def isMedia(request: Request, path: String): Boolean =
    val destination = request.destination.toString
    destination == "image" ||
    destination == "font" ||
    path.endsWith(".png") ||
    path.endsWith(".jpg") ||
    path.endsWith(".jpeg") ||
    path.endsWith(".svg") ||
    path.endsWith(".webp") ||
    path.endsWith(".gif") ||
    path.endsWith(".woff") ||
    path.endsWith(".woff2")

  private def cacheFirst(cacheName: String, request: Request): Future[Response] =
    val maybeCacheStorage = self.caches
    if js.isUndefined(maybeCacheStorage) then
      fetch(request).toFuture
    else
      val cacheStorage = maybeCacheStorage.get
      cacheStorage
        .open(cacheName)
        .toFuture
        .flatMap { cache =>
          cache
            .`match`(request)
            .toFuture
            .flatMap {
              case cached: Response => Future.successful(cached)
              case _ =>
                fetch(request).toFuture.flatMap { networkResp =>
                  if networkResp != null && networkResp.ok then
                    cache.put(request, networkResp.clone()).toFuture.map(_ => networkResp)
                  else Future.successful(networkResp)
                }
            }
        }
        .recoverWith { case _ => fetch(request).toFuture }

  private def staleWhileRevalidate(
    cacheName: String,
    event: FetchEvent,
    request: Request,
  ): Future[Response] =
    val maybeCacheStorage = self.caches
    if js.isUndefined(maybeCacheStorage) then
      fetch(request).toFuture
    else
      val cacheStorage = maybeCacheStorage.get
      cacheStorage
        .open(cacheName)
        .toFuture
        .flatMap { cache =>
          cache
            .`match`(request)
            .toFuture
            .flatMap {
              case cached: Response =>
                val updateF =
                  fetch(request).toFuture
                    .flatMap { networkResp =>
                      if networkResp != null && networkResp.ok then
                        cache.put(request, networkResp.clone()).toFuture.map(_ => ())
                      else Future.unit
                    }
                    .recover { case _ => () }
                event.waitUntil(updateF.toJSPromise)
                Future.successful(cached)
              case _ =>
                fetch(request).toFuture.flatMap { networkResp =>
                  if networkResp != null && networkResp.ok then
                    cache.put(request, networkResp.clone()).toFuture.map(_ => networkResp)
                  else Future.successful(networkResp)
                }
            }
        }
        .recoverWith { case _ => fetch(request).toFuture }

  private def cleanupOldCaches(): Future[Unit] =
    val maybeCacheStorage = self.caches
    if js.isUndefined(maybeCacheStorage) then
      Future.unit
    else
      val cacheStorage = maybeCacheStorage.get
      cacheStorage
        .keys()
        .toFuture
        .flatMap { keys =>
          val deleteFs = keys.toSeq.collect {
            case key
                if cachePrefixes.exists(prefix => key.startsWith(prefix)) &&
                  key != staticCacheName &&
                  key != mediaCacheName =>
              cacheStorage.delete(key).toFuture.map(_ => ())
          }
          Future.sequence(deleteFs).map(_ => ())
        }
        .recover { case _ => () }
