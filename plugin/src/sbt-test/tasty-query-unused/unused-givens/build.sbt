Global / findUnusedUseLocalClasspath := true
Global / findUnusedPackages += "bondlink"

lazy val root = project.in(file("."))
  .settings(
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.6.3",
  )
