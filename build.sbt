ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
libraryDependencies += "org.scalafx" %% "scalafx" % "19.0.0-R30"

lazy val root = (project in file("."))
  .settings(
    name := "modular_synth"
  )
