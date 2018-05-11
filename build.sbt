import Dependencies._

resolvers += "ruimo.com" at "http://static.ruimo.com/release"

name := "FormBuilder"
fork in run := true
scalacOptions := Seq("-unchecked", "-deprecation", "-feature")
libraryDependencies ++= Seq(
  "org.scalafx" % "scalafx_2.12" % "8.0.102-R11",
  "com.ruimo" %% "scoins" % "1.14",
  "com.ruimo" %% "formbuildercommon" % "1.1-SNAPSHOT",
  "com.typesafe.play" %% "play-ahc-ws-standalone" % "1.1.8",
  "com.typesafe.play" %% "play-ws-standalone-json" % "1.1.8",
  "com.typesafe.play" % "play-json_2.12" % "2.6.8",
  "com.ruimo" %% "graphics" % "1.10",
  "com.typesafe" % "config" % "1.3.1",
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "ch.qos.logback" % "logback-core" % "1.2.3",
  "org.specs2" %% "specs2-core" % "4.0.0" % "test"
)

lazy val root = (project in file(".")).
  enablePlugins(JavaAppPackaging, BuildInfoPlugin).
  settings(
    organization := "com.functionalcapture",
    scalaVersion := "2.12.4",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "generated"
  )

publishTo := Some(
  Resolver.file(
    "formbuilder",
    new File(Option(System.getenv("RELEASE_DIR")).getOrElse("/tmp"))
  )
)
