Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")

ThisBuild / scalaVersion := sys.props("scala.version")

lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
)

lazy val macros = project.in(file("macros")).settings(commonSettings)
lazy val app = project.in(file("app")).settings(commonSettings).dependsOn(macros).aggregate(macros)
