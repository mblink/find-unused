Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")

version := "0.1.0-SNAPSHOT"
scalaVersion := "3.7.3"
libraryDependencies += "io.circe" %% "circe-core" % "0.14.12"
