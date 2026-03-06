import sbt._
import scala.sys.process._
import scala.util.Try

/** Ngrok tunnel management for local development with Slack OAuth. */
object Ngrok {
  private val NgrokApi = "http://localhost:4040/api/tunnels"

  /** Start ngrok if not already running, return the HTTPS public URL. */
  def ensureTunnel(port: Int, log: Logger): Option[String] = {
    // Check if ngrok is already running
    val result = getPublicUrl(log) match {
      case Some(url) =>
        Some(url)
      case None =>
        startNgrok(port, log)
        waitForTunnel(log, maxAttempts = 30)
    }

    // Always print prominent banner with callback URL
    result.foreach { url =>
      log.info("")
      log.info("=" * 60)
      log.info(s"  NGROK TUNNEL: $url")
      log.info(s"  SLACK CALLBACK: $url/slack/callback")
      log.info("=" * 60)
      log.info("")
    }

    result
  }

  /** Get the current ngrok HTTPS URL from the local API. */
  def getPublicUrl(log: Logger): Option[String] = {
    Try {
      val response = scala.io.Source.fromURL(NgrokApi).mkString
      // Parse JSON manually to avoid adding dependencies in project/
      val httpsPattern = """"public_url"\s*:\s*"(https://[^"]+)"""".r
      httpsPattern.findFirstMatchIn(response).map(_.group(1))
    }.toOption.flatten
  }

  private def startNgrok(port: Int, log: Logger): Unit = {
    // Check ngrok is installed
    if (Try("which ngrok".!!).isFailure) {
      log.error("ngrok not found. Install with: brew install ngrok")
      return
    }

    log.info(s"Starting ngrok tunnel to localhost:$port...")
    // Start ngrok in background - it detaches from sbt
    Process(Seq("ngrok", "http", port.toString)).run()
  }

  private def waitForTunnel(log: Logger, maxAttempts: Int): Option[String] = {
    for (_ <- 1 to maxAttempts) {
      getPublicUrl(log) match {
        case Some(url) => return Some(url)
        case None => Thread.sleep(500)
      }
    }
    log.error("Failed to get ngrok URL after timeout")
    None
  }

  /** Override APP_BASE_URL in an env map with the ngrok URL. */
  def overrideBaseUrl(envVars: Map[String, String], ngrokUrl: String): Map[String, String] = {
    envVars + ("APP_BASE_URL" -> ngrokUrl)
  }
}
