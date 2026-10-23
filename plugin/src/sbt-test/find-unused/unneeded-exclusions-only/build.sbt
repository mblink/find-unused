Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")
Global / findUnusedExclusions := Seq(
  // Matches nothing
  FindUnusedExclusion(sym = "^\\Qbl.unused.Removed\\E$".r),
)

version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := sys.props("scala.version")
