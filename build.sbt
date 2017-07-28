import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    unmanagedJars in Compile += {
        val ps = new sys.SystemProperties
        val jh = ps("java.home")
        Attributed.blank(file(jh) / "lib/ext/jfxrt.jar")
    },
    fork in run := true,
    libraryDependencies ++= Seq(
      "org.scalafx" % "scalafx_2.12" % "8.0.102-R11",
      "com.typesafe.play" %% "play-ahc-ws-standalone" % "1.0.2",
      "com.typesafe.play" %% "play-ws-standalone-json" % "1.0.2",
      scalaTest % Test
    )
  )
