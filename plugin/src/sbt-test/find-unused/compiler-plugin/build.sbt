Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"
Global / findUnusedOutputFile := Some((ThisBuild / baseDirectory).value / "output" / "actual.txt")
Global / findUnusedCompilerPlugin := true

ThisBuild / scalaVersion := sys.props("scala.version")

lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
)

lazy val a = project.in(file("a")).settings(commonSettings)
lazy val b = project.in(file("b")).settings(commonSettings).dependsOn(a).aggregate(a)

// Stands in for a class compiled without the compiler plugin
TaskKey[Unit]("deleteRecord") := IO.delete((b / findUnusedInlineGivenRecords).value.map(_ / "bl" / "unused" / "Uses.tsv"))

// Stands in for the record of `Uses` from when it was in `a`, which the compiler doesn't delete with its TASTy file
TaskKey[Unit]("addStaleRecord") :=
  IO.write((a / findUnusedInlineGivenRecords).value.head / "bl" / "unused" / "Uses.tsv", "")
