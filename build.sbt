import Dependencies._

resolvers += "ruimo.com" at "http://static.ruimo.com/release"

lazy val root = (project in file(".")).
   enablePlugins(JavaAppPackaging).
   settings(
    inThisBuild(List(
      organization := "site.functionalcapture",
      scalaVersion := "2.12.3",
      version      := "1.0.0-SNAPSHOT"
    )),
    name := "FormBuilder",
//    unmanagedJars in Compile += {
//        val ps = new sys.SystemProperties
//        val jh = ps("java.home")
//        Attributed.blank(file(jh) / "lib/ext/jfxrt.jar")
//    },
    fork in run := true,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies ++= Seq(
      "org.scalafx" % "scalafx_2.12" % "8.0.102-R11",
      "com.ruimo" %% "scoins" % "1.12",
      "com.typesafe.play" %% "play-ahc-ws-standalone" % "1.0.2",
      "com.typesafe.play" %% "play-ws-standalone-json" % "1.0.2",
      "com.typesafe.play" % "play-json_2.12" % "2.6.3",
      "com.ruimo" %% "graphics" % "1.2",
      "com.typesafe" % "config" % "1.3.1",
      "org.specs2" %% "specs2-core" % "4.0.0" % "test"
    )
  )
