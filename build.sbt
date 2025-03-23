ThisBuild / scalaVersion     := "3.3.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.billding"
ThisBuild / organizationName := "billding"

enablePlugins(DockerPlugin)

//Docker / mainClass := Some("com.example.MainClassName")

name := "sticky-icky"

import org.scalajs.linker.interface.ModuleSplitStyle

lazy val sharedCode =
  crossProject(JSPlatform, JVMPlatform).in(file("shared_code"))
  .settings(
    name := "shared",
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-json" % "0.7.38",
      "dev.zio" %%% "zio-schema"          % "1.6.4",
      "dev.zio" %%% "zio-schema-json"     % "1.6.4",
      "dev.zio" %% "zio-schema-derivation" % "1.6.4", // TODO Is this doing anything if we can't include the dep below for JS?
      "io.github.kitlangton" %%% "neotype" % "0.3.15",
      "io.github.kitlangton" %%% "neotype-zio-json" % "0.3.15",
      "dev.zio" %% "zio-test" % "2.1.16" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.1.16" % Test,
//      "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided"
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
  .enablePlugins(JavaAppPackaging, AshScriptPlugin)
  .dependsOn(sharedCode.jvm)
  .settings(
    name := "server",
    dockerExposedPorts ++= Seq(8080, 9000, 9001),
    dockerBaseImage := "eclipse-temurin:21",
    dockerUpdateLatest := true,
    dockerBuildxPlatforms := Seq("linux/arm64/v8", "linux/amd64"),
    dockerUsername := Some("swoogles"),
    Compile / mainClass := Some("co.wtf.openspaces.Backend"),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.16",
      "dev.zio" %% "zio-http" % "3.0.1",
      "dev.zio" %% "zio-direct" % "1.0.0-RC7"

    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )


lazy val client = (project in file("client"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(sharedCode.js)
  .settings(
    name := "client",
    Compile / fastOptJS / artifactPath := baseDirectory.value.getParentFile / "server" / "src" / "main" / "resources" / "public" / "client-fastopt.js",
    Compile / fullOptJS / artifactPath := baseDirectory.value.getParentFile / "server" / "src" / "main" / "resources" / "public" / "client-fastopt.js",
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
      "io.github.kitlangton" %%% "animus" % "0.6.5",
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