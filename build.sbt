Global / onChangedBuildSource := ReloadOnSourceChanges

// To develop tasty-query locally, run sbt with the environment variable `TASTY_QUERY_DEVELOPMENT=1`
lazy val tastyQuery = ProjectRef(file(sys.env("HOME")) / "tasty-query", "tastyQueryJVM")
lazy val tastyQueryDev = sys.env.get("TASTY_QUERY_DEVELOPMENT").exists(_ == "1")

lazy val root = project.in(file("."))
  .settings(
    name := "tasty-query-unused",
    organization := "bondlink",
    scalaVersion := "3.6.3",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= (
      if (tastyQueryDev) Seq()
      else Seq("ch.epfl.scala" %% "tasty-query" % "1.5.0")
    ) ++ Seq(
      "com.lihaoyi" %% "os-lib" % "0.11.3",
      "com.lihaoyi" %% "pprint" % "0.9.0",
      "org.typelevel" %% "cats-core" % "2.13.0",
    ),
    run / fork := true,
    run / javaOptions += "-Xmx8G"
  )
  .dependsOn((if (tastyQueryDev) Seq[ClasspathDep[ProjectReference]](tastyQuery) else Seq())*)
  .aggregate((if (tastyQueryDev) Seq(tastyQuery) else Seq())*)
