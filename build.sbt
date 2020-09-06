import Dependencies._

ThisBuild / scalaVersion := "2.13.2"
ThisBuild / version := "0.0.1"

lazy val root = (project in file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "zsync",
    libraryDependencies += "dev.zio" %% "zio" % "1.0.1",
    libraryDependencies += "dev.zio" %% "zio-nio" % "1.0.0-RC9",
    libraryDependencies += "dev.zio" %% "zio-process" % "0.1.0",
  )
