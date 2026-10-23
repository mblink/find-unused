Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")
Global / findUnusedExclusions := Seq(
  FindUnusedExclusion(sym = "^\\Qbl.unused.Show.int\\E$".r),
  FindUnusedExclusion(src = "^\\Qb/src/main/scala/bondlink/UsesShow.scala\\E$".r),
  // Used by `UsesShow`, so this doesn't exclude anything. Transitive mode doesn't notice, since `UsesShow` is only used
  // because another exclusion keeps it
  FindUnusedExclusion(sym = "^\\Qbl.unused.Show.str\\E$".r),
  // Matches nothing
  FindUnusedExclusion(sym = "^\\Qbl.unused.Removed\\E$".r),
  // Only used by `Kept.deadCaller`, which is unused, so this only excludes anything in transitive mode
  FindUnusedExclusion(sym = "^\\Qbl.unused.Kept.helper\\E$".r),
)

ThisBuild / scalaVersion := sys.props("scala.version")

lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
)

lazy val a = project.in(file("a")).settings(commonSettings)
lazy val b = project.in(file("b")).settings(commonSettings).dependsOn(a).aggregate(a)
