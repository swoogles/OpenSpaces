import sbt._
import scala.sys.process._
import scala.util.Try

/** Caddy reverse proxy management for local HTTPS development. */
object Caddy {
  private val HttpsPort = 8443

  /** Start Caddy if not already running. */
  def ensureRunning(caddyfile: File, log: Logger): Unit = {
    if (isRunning) {
      log.info(s"Caddy already running on https://localhost:$HttpsPort")
    } else {
      startCaddy(caddyfile, log)
    }
    printBanner(log)
  }

  /** Check if something is listening on the HTTPS port. */
  private def isRunning: Boolean = {
    Try {
      val result = s"lsof -i :$HttpsPort".!!
      result.nonEmpty
    }.getOrElse(false)
  }

  private def startCaddy(caddyfile: File, log: Logger): Unit = {
    // Check caddy is installed
    if (Try("which caddy".!!).isFailure) {
      log.error("caddy not found. Install with: brew install caddy")
      return
    }

    if (!caddyfile.exists()) {
      log.error(s"Caddyfile not found at ${caddyfile.absolutePath}")
      return
    }

    log.info(s"Starting Caddy with ${caddyfile.getName}...")
    // Start caddy in background - it detaches from sbt
    Process(Seq("caddy", "run", "--config", caddyfile.absolutePath)).run()

    // Wait a moment for Caddy to bind the port
    Thread.sleep(1000)

    if (!isRunning) {
      log.warn("Caddy may not have started correctly - check for errors")
    }
  }

  private def printBanner(log: Logger): Unit = {
    log.info("")
    log.info("=" * 60)
    log.info(s"  LOCAL HTTPS: https://localhost:$HttpsPort")
    log.info(s"  (proxying to http://localhost:8080)")
    log.info("=" * 60)
    log.info("")
  }

  /** Stop Caddy if running. */
  def stop(log: Logger): Unit = {
    Try("pkill -f 'caddy run'".!)
    log.info("Caddy stopped")
  }
}
