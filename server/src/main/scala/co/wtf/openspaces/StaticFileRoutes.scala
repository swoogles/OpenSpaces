package co.wtf.openspaces

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import zio.http.*

object StaticFileRoutes:

  private def resourceBytes(path: String): Array[Byte] =
    val stream = Option(getClass.getClassLoader.getResourceAsStream(path))
      .getOrElse(throw new RuntimeException(s"Missing resource: $path"))
    try stream.readAllBytes()
    finally stream.close()

  private def resourceText(path: String): String =
    String(resourceBytes(path), StandardCharsets.UTF_8)

  private def sha256Hex(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .map("%02x".format(_))
      .mkString

  private def immutableAssetUrl(path: String): String =
    val hash = sha256Hex(resourceBytes(s"public/$path")).take(16)
    s"/$path?v=$hash"

  private val stylesUrl = immutableAssetUrl("styles.css")
  private val clientJsUrl = immutableAssetUrl("client-fastopt.js")

  println(
    s"Static asset fingerprints: styles=$stylesUrl clientJs=$clientJsUrl",
  )

  private val indexHtml: String =
    resourceText("public/index.html")
      .replace("__STYLES_URL__", stylesUrl)
      .replace("__CLIENT_JS_URL__", clientJsUrl)

  private def withNoStore(response: Response): Response =
    response
      .addHeader(
        Header.Custom("Cache-Control", "no-store, max-age=0, must-revalidate"),
      )
      .addHeader(Header.Custom("Pragma", "no-cache"))
      .addHeader(Header.Custom("Expires", "0"))

  private def withImmutableCache(response: Response): Response =
    response.addHeader(
      Header.Custom("Cache-Control", "public, max-age=31536000, immutable"),
    )

  val routes: Routes[Any, Response] =
    Routes(
      Method.GET / "" -> handler(
        withNoStore(
          Response
            .text(indexHtml)
            .addHeader(Header.ContentType(MediaType.text.html)),
        ),
      ),
      Method.HEAD / "" -> handler(
        withNoStore(
          Response
            .text(indexHtml)
            .addHeader(Header.ContentType(MediaType.text.html)),
        ),
      ),
      Method.GET / "index.html" -> handler(
        withNoStore(
          Response
            .text(indexHtml)
            .addHeader(Header.ContentType(MediaType.text.html)),
        ),
      ),
      Method.HEAD / "index.html" -> handler(
        withNoStore(
          Response
            .text(indexHtml)
            .addHeader(Header.ContentType(MediaType.text.html)),
        ),
      ),
      Method.GET / "client-fastopt.js" -> handler(
        Handler
          .fromResource("public/client-fastopt.js")
          .map(
            withImmutableCache(_)
              .addHeader(
                Header.Custom("Content-Type", "application/javascript; charset=utf-8"),
              ),
          )
          .mapError(e => Response.text(e.getMessage)),
      ),
      Method.HEAD / "client-fastopt.js" -> handler(
        Handler
          .fromResource("public/client-fastopt.js")
          .map(
            withImmutableCache(_)
              .addHeader(
                Header.Custom("Content-Type", "application/javascript; charset=utf-8"),
              ),
          )
          .mapError(e => Response.text(e.getMessage)),
      ),
      Method.GET / "styles.css" -> handler(
        Handler
          .fromResource("public/styles.css")
          .map(
            withImmutableCache(_)
              .addHeader(
                Header.Custom("Content-Type", "text/css; charset=utf-8"),
              ),
          )
          .mapError(e => Response.text(e.getMessage)),
      ),
      Method.HEAD / "styles.css" -> handler(
        Handler
          .fromResource("public/styles.css")
          .map(
            withImmutableCache(_)
              .addHeader(
                Header.Custom("Content-Type", "text/css; charset=utf-8"),
              ),
          )
          .mapError(e => Response.text(e.getMessage)),
      ),
      Method.GET / "sw.js" -> handler(
        Handler
          .fromResource("public/sw.js")
          .map(
            withNoStore(_)
              .addHeader(
                Header.Custom("Content-Type", "application/javascript; charset=utf-8"),
              ),
          )
          .mapError(e => Response.text(e.getMessage)),
      ),
      Method.HEAD / "sw.js" -> handler(
        Handler
          .fromResource("public/sw.js")
          .map(
            withNoStore(_)
              .addHeader(
                Header.Custom("Content-Type", "application/javascript; charset=utf-8"),
              ),
          )
          .mapError(e => Response.text(e.getMessage)),
      ),
      Method.GET / "manifest.webmanifest" -> handler(
        Handler
          .fromResource("public/manifest.webmanifest")
          .map(
            withNoStore(_)
              .addHeader(
                Header.Custom(
                  "Content-Type",
                  "application/manifest+json; charset=utf-8",
                ),
              ),
          )
          .mapError(e => Response.text(e.getMessage)),
      ),
      Method.HEAD / "manifest.webmanifest" -> handler(
        Handler
          .fromResource("public/manifest.webmanifest")
          .map(
            withNoStore(_)
              .addHeader(
                Header.Custom(
                  "Content-Type",
                  "application/manifest+json; charset=utf-8",
                ),
              ),
          )
          .mapError(e => Response.text(e.getMessage)),
      ),
    )
