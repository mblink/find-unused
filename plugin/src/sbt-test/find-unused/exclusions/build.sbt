Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")
Global / findUnusedExclusions := Seq(
  FindUnusedExclusion(sym = "^\\Qbl.unused.Show.int\\E$".r),
  FindUnusedExclusion(src = "^\\Qb/src/main/scala/bondlink/UsesShow.scala\\E$".r),
)

lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "3.6.4",
)

lazy val a = project.in(file("a")).settings(commonSettings)
lazy val b = project.in(file("b")).settings(commonSettings).dependsOn(a).aggregate(a)
