ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "shapeless-play-json",
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json"                % "2.9.3",
      "com.chuusai"       %% "shapeless"                % "2.3.10",
      "org.scalatest"     %% "scalatest"                % "3.2.15" % "test"
    )
  )
