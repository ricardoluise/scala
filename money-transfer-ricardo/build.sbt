name := "money-transfer"

version := "0.1.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
  "org.slf4j" % "slf4j-api" % "1.7.20",
  "ch.qos.logback" % "logback-classic" % "1.1.7",

  "commons-io" % "commons-io" % "2.5",

  "io.netty" % "netty-all" % "4.0.36.Final",

  "com.typesafe.play" %% "play-json" % "2.5.3",

  "com.wordnik" % "swagger-annotations" % "1.3.10",
  "javax.ws.rs" % "jsr311-api" % "1.1.1",


  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"
)

mainClass in Compile := Some("org.ricardo.mt.MTServerApp")
mainClass in assembly := Some("org.ricardo.mt.MTServerApp")
