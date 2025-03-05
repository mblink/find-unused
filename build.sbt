Global / onChangedBuildSource := ReloadOnSourceChanges

// To develop tasty-query locally, run sbt with the environment variable `TASTY_QUERY_DEVELOPMENT=1`
lazy val tastyQuery = ProjectRef(file(sys.env("HOME")) / "tasty-query", "tastyQueryJVM")
lazy val tastyQueryDev = sys.env.get("TASTY_QUERY_DEVELOPMENT").exists(_ == "1")

lazy val scala2 = "2.12.20"
lazy val scala36 = "3.6.3"

ThisBuild / crossScalaVersions := Seq(scala2, scala36)

val java21 = JavaSpec.graalvm(Graalvm.Distribution("graalvm"), "21")
val javaVersions = Seq(JavaSpec.temurin("11"), JavaSpec.temurin("17"), java21)

val ubuntuLatest = "ubuntu-latest"
val githubOSes = List(
  ubuntuLatest -> "linux",
  "ubuntu-24.04-arm" -> "linux-arm",
  "macos-13" -> "mac",
  "macos-latest" -> "mac-arm",
  "windows-latest" -> "windows",
)

val cliArtifacts = "cli/artifacts/"
def cliExt(osShort: String): String = if (osShort == "windows") ".exe" else ""
def cliName(osShort: String): String = "find-unused-" ++ osShort ++ cliExt(osShort)
def cliPath(osShort: String): String = cliArtifacts ++ cliName(osShort)

ThisBuild / githubWorkflowPermissions := Some(Permissions.Specify(Map(
  PermissionScope.IdToken -> PermissionValue.Write,
  PermissionScope.Attestations -> PermissionValue.Write,
)))
ThisBuild / githubWorkflowBuildMatrixInclusions := githubOSes.map { case (long, short) =>
  MatrixInclude(
    Map("os" -> long),
    Map(
      "cli_input_path" -> ("cli/target/graalvm-native-image/find-unused-cli" ++ cliExt(short)),
      "cli_output_name" -> cliName(short),
      "cli_output_path" -> cliPath(short),
    ),
  )
}
ThisBuild / githubWorkflowOSes := githubOSes.map(_._1)
ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("main", "native-image")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def isJava(v: Int) = s"matrix.java == '${javaVersions.find(_.version == v.toString).get.render}'"
def isScala(v: String) = s"matrix.scala == '$v'"

val java21AndScala36 = isJava(21) ++ " && " ++ isScala(scala36)

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("test", "scripted"), name = Some("Test")),
  WorkflowStep.Sbt(List("cli/GraalVMNativeImage/packageBin"), name = Some("Build CLI"), cond = Some(java21AndScala36)),
  WorkflowStep.Run(
    List(
      s"mkdir -p $cliArtifacts",
      "cp ${{ matrix.cli_input_path }} ${{ matrix.cli_output_path }}",
    ),
    name = Some("Copy CLI"),
    cond = Some(java21AndScala36),
  ),
  WorkflowStep.Use(
    ref = UseRef.Public("actions", "attest-build-provenance", "v2"),
    name = Some("Attest CLI"),
    cond = Some(java21AndScala36),
    params = Map("subject-path" -> "cli/artifacts/${{ matrix.cli_output_name }}"),
  ),
  WorkflowStep.Use(
    ref = UseRef.Public("actions", "upload-artifact", "v4"),
    name = Some("Upload CLI"),
    cond = Some(java21AndScala36),
    params = Map(
      "name" -> "${{ matrix.cli_output_name }}",
      "path" -> cliArtifacts,
      "if-no-files-found" -> "error",
      "retention-days" -> "2",
    )
  ),
)

ThisBuild / githubWorkflowAddedJobs += WorkflowJob(
  id = "release",
  name = "Release",
  oses = List(ubuntuLatest),
  javas = List(java21),
  scalas = List(scala36),
  cond = Some("startsWith(github.ref, 'refs/tags/v')"),
  needs = List("build"),
  steps = List(WorkflowStep.CheckoutFull) ++
    githubOSes.map { case (_, short) =>
      WorkflowStep.Use(
        ref = UseRef.Public("actions", "download-artifact", "v4"),
        params = Map(
          "name" -> cliName(short),
          "path" -> cliArtifacts,
        )
      )
    } ++
    List(
      WorkflowStep.Use(
        ref = UseRef.Public("softprops", "action-gh-release", "v2"),
        name = Some("Create Release"),
        params = Map(
          "draft" -> "true",
          "files" -> githubOSes.map { case (_, short) => cliPath(short) }.mkString("\n"),
          "fail_on_unmatched_files" -> "true",
        ),
      ),
    )
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
