 resolvers += Resolver.mavenLocal

  addSbtPlugin("io.spray" % "sbt-revolver" % "0.10.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.18.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.1")
// NOTE: sbt-dotenv plugin hacks java.lang.ProcessEnvironment (breaks on JDK 17/21).
// We use a tiny project-local Dotenv loader instead (see project/Dotenv.scala).
// addSbtPlugin("io.github.swoogles" % "sbt-codeartifact" % "0.25.0")
