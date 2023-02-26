ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "modular_synth"
  )
