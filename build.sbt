import Dependencies._

resolvers += "ruimo.com" at "http://static.ruimo.com/release"

lazy val root = (project in file(".")).
   enablePlugins(JavaAppPackaging, BuildInfoPlugin).
   settings(
    inThisBuild(List(
      organization := "site.functionalcapture",
      scalaVersion := "2.12.4",
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "generated"
    )),
    name := "FormBuilder",
    fork in run := true,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies ++= Seq(
      "org.scalafx" % "scalafx_2.12" % "8.0.102-R11",
      "com.ruimo" %% "scoins" % "1.13",
      "com.typesafe.play" %% "play-ahc-ws-standalone" % "1.1.3",
      "com.typesafe.play" %% "play-ws-standalone-json" % "1.1.3",
      "com.typesafe.play" % "play-json_2.12" % "2.6.8",
      "com.ruimo" %% "graphics" % "1.2",
      "com.typesafe" % "config" % "1.3.1",
      "org.specs2" %% "specs2-core" % "4.0.0" % "test"
    )
  )

publishTo := Some(
  Resolver.file(
    "formbuilder",
    new File(Option(System.getenv("RELEASE_DIR")).getOrElse("/tmp"))
  )
)
