Global / findUnusedUseLocalClasspath := true
Global / findUnusedPackages += "bondlink"

lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "3.6.4",
)

lazy val a = project.in(file("a")).settings(commonSettings)
lazy val b = project.in(file("b")).settings(commonSettings).dependsOn(a).aggregate(a)
