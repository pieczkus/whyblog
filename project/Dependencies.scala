import sbt._
import Keys._

object Dependencies {
  val akkaVersion = "2.5.3"
  val akkaCluster = "com.typesafe.akka" %% "akka-cluster" % akkaVersion
  val akkaClusterTools = "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion
  val akkaClusterSharding = "com.typesafe.akka" %% "akka-cluster-sharding" % akkaVersion
  val akkaRemote = "com.typesafe.akka" %% "akka-remote" % akkaVersion
  val akkaPersistence = "com.typesafe.akka" %% "akka-persistence" % akkaVersion
  val akkaPersistenceCassandra = "com.typesafe.akka" %% "akka-persistence-cassandra" % "0.51"
  val akkaPersistenceQuery = "com.typesafe.akka" %% "akka-persistence-query" % akkaVersion
  val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion

  val elastic4sVersion = "5.2.11"
  val elastic4s = "com.sksamuel.elastic4s" %% "elastic4s-core" % elastic4sVersion
  val elastic4sXpack = "com.sksamuel.elastic4s" %% "elastic4s-xpack-security" % elastic4sVersion

  val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.1.3"
  val jwt = "com.nimbusds" % "nimbus-jose-jwt" % "4.7"

  val json4sVersion = "3.5.2"
  val json4sNative = "org.json4s" %% "json4s-native" % json4sVersion
  val json4sExt = "org.json4s" %% "json4s-ext" % json4sVersion

  val proto = "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion

  val bcrypt = "com.github.t3hnar" % "scala-bcrypt_2.11" % "2.5"

  val scalaUri = "com.netaporter" %% "scala-uri" % "0.4.16"
  val scalaGuice = "net.codingwell" %% "scala-guice" % "4.1.0"

  val playTest = "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0" % Test
  val gatlingCharts = "io.gatling.highcharts" % "gatling-charts-highcharts" % "2.2.2" % Test
  val gatling = "io.gatling" % "gatling-test-framework" % "2.2.2" % Test

  val jodaConvert = "org.joda" % "joda-convert" % "1.8.1"

  val whyCommon = "pl.why" %% "common" % "1.1"

  val playDependencies: Seq[ModuleID] = Seq(
    scalaGuice,
    scalaUri,
    gatling,
    gatlingCharts
  )

  val akkaDependencies: Seq[ModuleID] = Seq(
    akkaCluster,
    akkaClusterSharding,
    akkaRemote,
    akkaPersistenceCassandra,
    akkaSlf4j,
    logbackClassic
  )

  val arrDependencies: Seq[ModuleID] = Seq(
    whyCommon,
    proto,
    elastic4s,
    elastic4sXpack,
    jodaConvert,
    json4sNative,
    json4sExt
  )
}

