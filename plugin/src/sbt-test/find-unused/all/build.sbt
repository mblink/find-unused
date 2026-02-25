Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")

lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "3.8.2",
)

lazy val a = project.in(file("a")).settings(commonSettings)
lazy val b = project.in(file("b")).settings(commonSettings).dependsOn(a).aggregate(a)
