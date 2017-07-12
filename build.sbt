import sbt.Keys.{libraryDependencies, _}
import com.typesafe.sbt.packager.docker._
import Dependencies._

lazy val GatlingTest = config("gatling") extend Test

scalaVersion := "2.11.8"

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

// The Play project itself
lazy val root = (project in file("."))
  .enablePlugins(Common, PlayScala, GatlingPlugin, JavaAppPackaging, DockerPlugin)
  .configs(GatlingTest)
  .settings(inConfig(GatlingTest)(Defaults.testSettings): _*)
  .settings(PB.protoSources in Compile += baseDirectory.value / "protobuf")
  .settings(
    name := """why-blog""",
    scalaSource in GatlingTest := baseDirectory.value / "/gatling/simulation",
    libraryDependencies ++= playDependencies,
    libraryDependencies ++= akkaDependencies,
    libraryDependencies ++= arrDependencies,
    libraryDependencies += guice
  )

// --------------------
// ------ DOCKER ------
// --------------------
// build with activator docker:publishLocal

// change to smaller base image
dockerBaseImage := "frolvlad/alpine-oraclejdk8:latest"
dockerCommands := dockerCommands.value.flatMap {
  case cmd@Cmd("FROM", _) => List(cmd, Cmd("RUN", "apk update && apk add bash"))
  case other => List(other)
}

// setting a maintainer which is used for all packaging types</pre>
maintainer := "Bart Pieczka"

// exposing the play ports
dockerExposedPorts in Docker := Seq(9000)
