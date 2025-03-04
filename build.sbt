ThisBuild / scalaVersion     := "3.3.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val server = (project in file("server"))
  .settings(
    name := "server",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.11",
      "dev.zio" %% "zio-test" % "2.1.11" % Test,
      "dev.zio" %% "zio-http" % "3.0.1",
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

import org.scalajs.linker.interface.ModuleSplitStyle
lazy val client = (project in file("client"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "client",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("livechart")))
    },
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio" % "2.1.11",
      "dev.zio" %%% "zio-test" % "2.1.11" % Test,
//      "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo" %%% "laminar" % "17.2.0" // Requires Scala.js 1.16.0+
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

lazy val root = (project in file("."))
  .aggregate(client, server)


/*
lazy val root = (project in file("."))
  .aggregate(core, util)

lazy val core = (project in file("core"))
lazy val util = (project in file("util"))

 */

Global / onChangedBuildSource := ReloadOnSourceChanges