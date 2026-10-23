Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")
Global / findUnusedExclusions := Seq(
  // Only used by code that isn't analyzed, which needs `Config.Section` and `Config` too
  FindUnusedExclusion(sym = "^\\Qbl.unused.Config.Section.empty\\E$".r),
)

ThisBuild / scalaVersion := sys.props("scala.version")

lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
)

lazy val a = project.in(file("a")).settings(commonSettings)
lazy val b = project.in(file("b")).settings(commonSettings).dependsOn(a).aggregate(a)
