ThisBuild / scalaVersion     := "3.3.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

import org.scalajs.linker.interface.ModuleSplitStyle

lazy val sharedCode =
  crossProject(JSPlatform, JVMPlatform).in(file("shared_code"))
  .settings(
    name := "shared",
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-json" % "0.7.38",
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
.jsSettings(
  scalaJSLinkerConfig ~= {
    _.withModuleKind(ModuleKind.ESModule)
      .withModuleSplitStyle(
        ModuleSplitStyle.SmallModulesFor(List("livechart")))
  })

lazy val server = (project in file("server"))
  .dependsOn(sharedCode.jvm)
  .settings(
    name := "server",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.16",
      "dev.zio" %% "zio-test" % "2.1.16" % Test,
      "dev.zio" %% "zio-http" % "3.0.1",
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )


lazy val client = (project in file("client"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(sharedCode.js)
  .settings(
    name := "client",
    Compile / fastOptJS / artifactPath := baseDirectory.value.getParentFile / "server" / "src" / "main" / "resources" / "public" / "client-fastopt.js",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("livechart")))
    },
    libraryDependencies ++= Seq(
//      "dev.zio" %%% "zio" % "2.1.11",
      "dev.zio" %%% "zio-test" % "2.1.11" % Test,
      "dev.zio" %%% "zio-json" % "0.7.38",
//      "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      // build.sbt
      "io.github.kitlangton" %%% "animus" % "0.5.1",
      "com.raquo" %%% "laminar" % "17.2.0",
      "dev.laminext" %%% "websocket" % "0.17.1"
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