import sbt._

/** Minimal .env loader for sbt.
  *
  * Why: sbt-dotenv plugins commonly try to mutate the JVM process environment via reflection
  * (java.lang.ProcessEnvironment), which breaks on modern JDKs.
  */
object Dotenv {
  def load(file: File, log: Logger): Map[String, String] = {
    if (!file.exists()) {
      log.info(s"Dotenv: no ${file.getAbsolutePath} found")
      Map.empty
    } else {
      val lines = IO.readLines(file)
      val kvs = lines.iterator
        .map(_.trim)
        .filter(l => l.nonEmpty && !l.startsWith("#"))
        .flatMap { line =>
          // allow `export KEY=VALUE`
          val noExport = line.stripPrefix("export ").trim
          val idx = noExport.indexOf('=')
          if (idx <= 0) None
          else {
            val k = noExport.substring(0, idx).trim
            val raw = noExport.substring(idx + 1).trim
            val v = unquote(raw)
            Some(k -> v)
          }
        }
        .toMap

      log.info(s"Dotenv: loaded ${kvs.size} vars from ${file.getAbsolutePath}")
      kvs
    }
  }

  private def unquote(s: String): String = {
    if (s.length >= 2 && ((s.startsWith("\"") && s.endsWith("\"")) || (s.startsWith("'") && s.endsWith("'"))))
      s.substring(1, s.length - 1)
    else s
  }
}
