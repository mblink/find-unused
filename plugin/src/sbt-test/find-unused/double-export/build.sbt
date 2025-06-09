Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")

lazy val root = project.in(file(".")).settings(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "3.7.1",
)
