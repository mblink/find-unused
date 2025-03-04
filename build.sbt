Global / onChangedBuildSource := ReloadOnSourceChanges

// To develop tasty-query locally, run sbt with the environment variable `TASTY_QUERY_DEVELOPMENT=1`
lazy val tastyQuery = ProjectRef(file(sys.env("HOME")) / "tasty-query", "tastyQueryJVM")
lazy val tastyQueryDev = sys.env.get("TASTY_QUERY_DEVELOPMENT").exists(_ == "1")

lazy val scala2 = "2.12.20"
lazy val scala36 = "3.6.3"

ThisBuild / crossScalaVersions := Seq(scala2, scala36)

val javaVersions = Seq(
  JavaSpec.temurin("11"),
  JavaSpec.temurin("17"),
  JavaSpec.graalvm(Graalvm.Distribution("graalvm"), "21"),
)

ThisBuild / githubWorkflowOSes := Seq(
  "ubuntu-latest",
  "ubuntu-24.04-arm",
  "macos-13", // x64
  "macos-latest", // ARM
  "windows-latest",
)
ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("main")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def isJava(v: Int) = s"matrix.java == '${javaVersions.find(_.version == v.toString).get.render}'"
def isScala(v: String) = s"matrix.scala == '$v'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("test", "scripted"), name = Some("Test")),
  WorkflowStep.Sbt(
    List("cli/GraalVMNativeImage/packageBin"),
    name = Some("Build CLI"),
    cond = Some(isJava(21) ++ " && " ++ isScala(scala36)),
  ),
)

lazy val commonSettings = Seq(
  organization := "bondlink",
  scalaVersion := scala36,
  crossScalaVersions := Seq(scala36),
  licenses += License.Apache2,
  publish / skip := true,
)

commonSettings
gitRelease := {}

lazy val publishSettings = Seq(
  publish / skip := false,
  gitPublishDir := file("/src/maven-repo"),
)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "find-unused-core",
    libraryDependencies ++= (
      if (tastyQueryDev) Seq()
      else Seq("ch.epfl.scala" %% "tasty-query" % "1.5.0")
    ) ++ Seq(
      "com.lihaoyi" %% "pprint" % "0.9.0",
      "org.typelevel" %% "cats-core" % "2.13.0",
    ),
  )
  .dependsOn((if (tastyQueryDev) Seq[ClasspathDep[ProjectReference]](tastyQuery) else Seq())*)
  .aggregate((if (tastyQueryDev) Seq(tastyQuery) else Seq())*)

lazy val cli = project.in(file("cli"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "find-unused-cli",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "mainargs" % "0.7.6",
    ),
    run / fork := true,
    graalVMNativeImageOptions += "--no-fallback",
  )
  .dependsOn(core)
  .aggregate(core)
  .enablePlugins(GraalVMNativeImagePlugin)

lazy val cliClasspath = taskKey[Seq[File]]("CLI classpath")

lazy val plugin = project.in(file("plugin"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "find-unused-plugin",
    scalaVersion := scala2,
    // Waiting on sbt 2.0.0-M4 to enable Scala 3.6, we need `Def.inputTaskDyn` which landed in https://github.com/sbt/sbt/pull/8033
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
