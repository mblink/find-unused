Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version := "0.4.0"

// To develop tasty-query locally, run sbt with the environment variable `TASTY_QUERY_DEVELOPMENT=1`
lazy val tastyQuery = ProjectRef(file(sys.env("HOME")) / "tasty-query", "tastyQueryJVM")
lazy val tastyQueryDev = sys.env.get("TASTY_QUERY_DEVELOPMENT").exists(_ == "1")

lazy val scala2 = "2.12.20"
lazy val scala37 = "3.7.3"

ThisBuild / crossScalaVersions := Seq(scala2, scala37)

val java25 = JavaSpec.temurin("25")
val javaVersions = Seq(JavaSpec.temurin("11"), JavaSpec.temurin("17"), JavaSpec.temurin("21"), java25)

val isTag = "startsWith(github.ref, 'refs/tags/v')"

val cliAssemblyJarNameEnv = "FIND_UNUSED_CLI_ASSEMBLY_JAR_NAME"

val cliArtifacts = "cli/artifacts/"

val cliName = "${{ format('find-unused-{0}.jar" ++ "', " ++ isTag ++ " && github.ref_name || github.sha) }}"
val cliNameNoSuffix = "find-unused.jar"

val cliPath = cliArtifacts ++ cliName
val cliPathNoSuffix = cliArtifacts ++ cliNameNoSuffix

ThisBuild / githubWorkflowPermissions := Some(Permissions.Specify(Map(
  PermissionScope.Attestations -> PermissionValue.Write,
  PermissionScope.Contents -> PermissionValue.Write,
  PermissionScope.IdToken -> PermissionValue.Write,
)))
ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("main")
ThisBuild / githubWorkflowTargetTags := Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def isJava(v: Int) = s"matrix.java == '${javaVersions.find(_.version == v.toString).get.render}'"
def isScala(v: String) = s"matrix.scala == '$v'"

val shouldBuildCLI = isJava(25) ++ " && " ++ isScala(scala37) ++ " && github.event_name == 'push'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("test", "scripted"), name = Some("scripted"), cond = Some(isScala(scala2))),
  WorkflowStep.Sbt(List("Test/compile"), name = Some("compile"), cond = Some(isScala(scala37))),
  WorkflowStep.Sbt(
    List("cli/assembly"),
    name = Some("Build CLI"),
    cond = Some(shouldBuildCLI),
    env = Map(cliAssemblyJarNameEnv -> cliName)
  ),
  WorkflowStep.Run(
    List(
      s"mkdir -p $cliArtifacts",
      "cp cli/target/scala-${{ matrix.scala }}/" ++ cliName ++ " " ++ cliPath,
    ),
    name = Some("Copy CLI"),
    cond = Some(shouldBuildCLI),
  ),
  WorkflowStep.Use(
    ref = UseRef.Public("actions", "attest-build-provenance", "v2"),
    name = Some("Attest CLI"),
    cond = Some(shouldBuildCLI),
    params = Map("subject-path" -> cliPath),
  ),
  WorkflowStep.Use(
    ref = UseRef.Public("actions", "upload-artifact", "v4"),
    name = Some("Upload CLI"),
    cond = Some(shouldBuildCLI),
    params = Map(
      "name" -> cliName,
      "path" -> cliArtifacts,
      "if-no-files-found" -> "error",
      "retention-days" -> "2",
    )
  ),
)

ThisBuild / githubWorkflowAddedJobs += WorkflowJob(
  id = "release",
  name = "Release",
  oses = List("ubuntu-latest"),
  javas = List(java25),
  scalas = List(scala37),
  cond = Some(isTag ++ " && github.event_name == 'push'"),
  needs = List("build"),
  steps = List(
    WorkflowStep.CheckoutFull,
    WorkflowStep.Use(
      ref = UseRef.Public("actions", "download-artifact", "v4"),
      name = Some("Download CLI"),
      params = Map("name" -> cliName, "path" -> cliArtifacts),
    ),
    WorkflowStep.Run(
      List(s"cp $cliPath $cliPathNoSuffix"),
      name = Some("Copy CLI"),
    ),
    WorkflowStep.Use(
      ref = UseRef.Public("softprops", "action-gh-release", "v2"),
      name = Some("Create Release"),
      params = Map(
        "draft" -> "true",
        "files" -> cliPathNoSuffix,
        "fail_on_unmatched_files" -> "true",
      ),
    ),
  ),
)

lazy val commonSettings = Seq(
  organization := "bondlink",
  scalaVersion := scala37,
  crossScalaVersions := Seq(scala37),
  licenses += License.Apache2,
  publish / skip := true,
)

commonSettings

lazy val publishSettings = Seq(
  publish / skip := false,
  publishTo := Some("BondLink S3".at("s3://bondlink-maven-repo")),
)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "find-unused-core",
    libraryDependencies ++= (
      if (tastyQueryDev) Seq()
      else Seq("ch.epfl.scala" %% "tasty-query" % "1.6.1")
    ) ++ Seq(
      "com.lihaoyi" %% "pprint" % "0.9.4",
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
      "com.lihaoyi" %% "mainargs" % "0.7.7",
      "org.jline" % "jline" % "3.30.6",
    ),
    run / fork := true,
    assembly / aggregate := false,
    assembly / mainClass := Some("bl.unused.FindUnusedCli"),
  )
  .settings(sys.env.get(cliAssemblyJarNameEnv) match {
    case Some(name) => Seq(assembly / assemblyJarName := name)
    case None => Seq.empty
  })
  .dependsOn(core)
  .aggregate(core)

lazy val cliClasspath = taskKey[Seq[File]]("CLI classpath")

def pluginSbtVersion(scalaBinaryVersion: String, sbt1Version: String): String =
  scalaBinaryVersion match {
    case "2.12" => sbt1Version
    case _ => "2.0.0-RC6"
  }

lazy val plugin = project.in(file("plugin"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "find-unused-plugin",
    scalaVersion := scala2,
    crossScalaVersions := Seq(scala2, scala37),
    publishConfiguration := publishConfiguration.value.withOverwrite(scalaVersion.value == scala37),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(scalaVersion.value == scala37),
    pluginCrossBuild / sbtVersion := pluginSbtVersion(scalaBinaryVersion.value, "1.9.0"),
    scriptedBufferLog := false,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    scriptedSbt := pluginSbtVersion(scalaBinaryVersion.value, sbtVersion.value),
    libraryDependencies ++= Seq(
      ("io.get-coursier" %% "coursier" % "2.1.24").cross(CrossVersion.for3Use2_13)
        .exclude("org.scala-lang.modules", "scala-xml_2.13"),
    ),
    cliClasspath := (cli / Runtime / fullClasspath).value.map(_.data),
    buildInfoKeys := Seq[BuildInfoKey](version, BuildInfoKey(cliClasspath)),
    buildInfoPackage := "bl.unused",
  )
  .enablePlugins(SbtPlugin, BuildInfoPlugin)
