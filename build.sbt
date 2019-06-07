ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.github.kmltml"
ThisBuild / organizationName := "kmltml"

lazy val root = (project in file("."))
  .settings(
    name := "buns-and-pups",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
    run / fork := true
  )
