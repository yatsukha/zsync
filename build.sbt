import Dependencies._

ThisBuild / scalaVersion := "2.13.2"
ThisBuild / version := "0.0.1"

lazy val root = (project in file("."))
  .settings(
    name := "remote-abuse",
    graalVMNativeImageOptions := Seq(
      "--initialize-at-build-time=scala.runtime.Statics$VM",
      "--no-fallback"
    ),
    libraryDependencies += "dev.zio" %% "zio"         % "1.0.0",
    libraryDependencies += "dev.zio" %% "zio-streams" % "1.0.0"
  )

enablePlugins(GraalVMNativeImagePlugin)