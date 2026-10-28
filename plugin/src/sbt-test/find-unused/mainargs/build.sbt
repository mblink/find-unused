Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")

version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := sys.props("scala.version")
libraryDependencies += "com.lihaoyi" %% "mainargs" % "0.7.8"
