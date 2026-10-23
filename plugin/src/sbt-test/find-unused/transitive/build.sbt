Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")
Global / findUnusedTransitive := true

ThisBuild / scalaVersion := sys.props("scala.version")

lazy val root = project.in(file(".")).settings(
  version := "0.1.0-SNAPSHOT",
)
