Global / onChangedBuildSource := ReloadOnSourceChanges

// To develop tasty-query locally, run sbt with the environment variable `TASTY_QUERY_DEVELOPMENT=1`
lazy val tastyQuery = ProjectRef(file(sys.env("HOME")) / "tasty-query", "tastyQueryJVM")
lazy val tastyQueryDev = sys.env.get("TASTY_QUERY_DEVELOPMENT").exists(_ == "1")

lazy val scala2 = "2.12.20"
lazy val scala33 = "3.3.5"
lazy val scala36 = "3.6.3"

lazy val commonSettings = Seq(
  organization := "bondlink",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := scala36,
  crossScalaVersions := Seq(/*scala33,*/ scala36),
)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "tasty-query-unused-core",
    libraryDependencies ++= (
      if (tastyQueryDev) Seq()
      else scalaVersion.value match {
        case `scala33` => Seq("ch.epfl.scala" %% "tasty-query" % "1.2.1")
        case `scala36` => Seq("ch.epfl.scala" %% "tasty-query" % "1.5.0")
      }
    ) ++ Seq(
      "com.lihaoyi" %% "pprint" % "0.9.0",
      "org.typelevel" %% "cats-core" % "2.13.0",
    ),
    run / fork := true,
    run / javaOptions += "-Xmx8G"
  )
  .dependsOn((if (tastyQueryDev) Seq[ClasspathDep[ProjectReference]](tastyQuery) else Seq())*)
  .aggregate((if (tastyQueryDev) Seq(tastyQuery) else Seq())*)

lazy val cli = project.in(file("cli"))
  .settings(commonSettings)
  .settings(
    name := "tasty-query-unused-cli",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "mainargs" % "0.7.6",
    ),
    run / fork := true,
  )
  .dependsOn(core)
  .aggregate(core)
  .enablePlugins(GraalVMNativeImagePlugin)

lazy val cliClasspath = taskKey[Seq[File]]("CLI classpath")

lazy val plugin = project.in(file("plugin"))
  .settings(commonSettings)
  .settings(
    name := "tasty-query-unused-plugin",
    crossScalaVersions := Seq(scala2/*, scala36*/),
    pluginCrossBuild / sbtVersion := {
      scalaBinaryVersion.value match {
        case "2.12" => "1.9.0"
        case _ => "2.0.0-M3"
      }
    },
    scriptedBufferLog := false,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    libraryDependencies ++= Seq(
      ("io.get-coursier" %% "coursier" % "2.1.24").cross(CrossVersion.for3Use2_13)
        .exclude("org.scala-lang.modules", "scala-xml_2.13")
    ),
    cliClasspath := (cli / Runtime / fullClasspath).value.map(_.data),
    buildInfoKeys := Seq[BuildInfoKey](version, BuildInfoKey(cliClasspath)),
    buildInfoPackage := "bondlink",
  )
  .enablePlugins(SbtPlugin, BuildInfoPlugin)
