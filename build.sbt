ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2022"
  )
libraryDependencies += "org.specs2" %% "specs2-core" % "4.17.0" % "test"
