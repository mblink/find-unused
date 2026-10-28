Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version := "0.7.1-SNAPSHOT"

// To develop tasty-query locally, run sbt with the environment variable `TASTY_QUERY_DEVELOPMENT=1`
lazy val tastyQuery = ProjectRef(file(sys.env("HOME")) / "tasty-query", "tastyQueryJVM")
lazy val tastyQueryDev = sys.env.get("TASTY_QUERY_DEVELOPMENT").exists(_ == "1")

lazy val scala2 = "2.12.21"
lazy val scala3ForLib = "3.10.0"
lazy val scala3ForSbt = "3.8.4"
lazy val scriptedScalaVersions = List("3.8.4", "3.9.0", scala3ForLib)
lazy val scriptedScalaVersion = sys.props.getOrElse("scripted.scala.version", scala3ForLib)

ThisBuild / scalaVersion := scala3ForLib

val java25 = JavaSpec.temurin("25")
val java17 = JavaSpec.temurin("17")
val javaVersions = List(java17, java25)

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

val shouldBuildCLI = isJava(25) ++ " && " ++ isScala(scala3ForLib) ++ " && github.event_name == 'push'"

// The scripted tests are split across this many runners, each running every `scriptedShards`th test
val scriptedShards = 2
val scriptedShardNames = (0 until scriptedShards).map(_.toString).toList

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(List("sbt test"), name = Some("Compile")),
  WorkflowStep.Run(List("python3 .claude/skills/validate-on-project/test_scripts.py"), name = Some("Check skill scripts")),
  WorkflowStep.Run(
    List("sbt cli3/assembly"),
    name = Some("Build CLI"),
    cond = Some(shouldBuildCLI),
    env = Map(cliAssemblyJarNameEnv -> cliName)
  ),
  WorkflowStep.Run(
    List(
      s"mkdir -p $cliArtifacts",
      "cp cli/target/jvm-3/" ++ cliName ++ " " ++ cliPath,
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
  id = "scripted",
  name = "Scripted tests",
  oses = List("ubuntu-latest"),
  javas = javaVersions,
  scalas = List(scala3ForLib),
  matrixFailFast = Some(false),
  matrixAdds = Map("scriptedScala" -> scriptedScalaVersions, "shard" -> scriptedShardNames),
  // Only test every scripted Scala version on the newest JDK; the oldest JDK just checks the latest Scala version
  matrixExcs = scriptedScalaVersions.filterNot(_ == scala3ForLib).map(v =>
    MatrixExclude(Map("java" -> java17.render, "scriptedScala" -> v))
  ),
  steps = List(WorkflowStep.CheckoutFull) ++ WorkflowStep.SetupJava(javaVersions) ++ List(
    WorkflowStep.SetupSbt(),
    WorkflowStep.Run(
      List(
        "tests=$(ls plugin/src/sbt-test/find-unused | sort | awk -v shard=${{ matrix.shard }} -v shards=" ++ scriptedShards.toString ++
          " '(NR - 1) % shards == shard' | sed 's#^#find-unused/#' | tr '\\n' ' ')",
        "echo \"Running: $tests\"",
        "sbt -Dscripted.scala.version=${{ matrix.scriptedScala }} \"scripted $tests\"",
      ),
      name = Some("scripted"),
    ),
  ),
)

ThisBuild / githubWorkflowAddedJobs += WorkflowJob(
  id = "release",
  name = "Release",
  oses = List("ubuntu-latest"),
  javas = List(java25),
  scalas = List(scala3ForLib),
  cond = Some(isTag ++ " && github.event_name == 'push'"),
  needs = List("build", "scripted"),
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
  licenses += License.Apache2,
  publish / skip := true,
)

commonSettings

lazy val publishSettings = Seq(
  publish / skip := false,
  s3PublishBucket := "bondlink-maven-repo",
)

def baseProj(id: String, nme: String, scalaVersions: Seq[String] = Seq(scala3ForLib)) =
  sbt.internal.ProjectMatrix(id, file(id))
    .jvmPlatform(scalaVersions = scalaVersions)
    .settings(commonSettings ++ Seq(name := nme))

lazy val core = baseProj("core", "find-unused-core")
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= (
      if (tastyQueryDev) Seq()
      else Seq("ch.epfl.scala" %% "tasty-query" % "1.10.0")
    ) ++ Seq(
      "com.lihaoyi" %% "pprint" % "0.9.6",
      "org.ow2.asm" % "asm" % "9.11.1",
      "org.typelevel" %% "cats-core" % "2.13.0",
    ),
  )
  .dependsOn((if (tastyQueryDev) Seq[ClasspathDep[ProjectReference]](tastyQuery) else Seq())*)
  .aggregate((if (tastyQueryDev) Seq(tastyQuery) else Seq())*)

lazy val cli = baseProj("cli", "find-unused-cli")
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "mainargs" % "0.7.8",
      "org.jline" % "jline" % "4.4.8",
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

// The scripts the skills run with scala-cli, compiled here so a change to `core` that breaks them fails the build
lazy val skills = sbt.internal.ProjectMatrix("skills", file(".claude/skills"))
  .jvmPlatform(scalaVersions = Seq(scala3ForLib))
  .settings(commonSettings)
  .settings(
    name := "find-unused-skills",
    libraryDependencies += "org.ow2.asm" % "asm-util" % "9.11.1",
    Compile / unmanagedSourceDirectories := Seq(),
    Compile / unmanagedSources := Seq(
      (ThisBuild / baseDirectory).value / ".claude" / "skills" / "benchmark" / "Bench.scala",
      (ThisBuild / baseDirectory).value / ".claude" / "skills" / "validate-on-project" / "CompareBytecode.scala",
      (ThisBuild / baseDirectory).value / ".claude" / "skills" / "validate-on-project" / "ExclusionsPrelude.scala",
    ),
    // `ExclusionsPrelude.scala` expects the exclusions copied from a project's build
    Compile / sourceGenerators += Def.task {
      val file = (Compile / sourceManaged).value / "Exclusions.scala"
      IO.write(file, "val exclusions: Seq[FindUnusedExclusion] = Seq(FindUnusedExclusion(sym = \"^x$\".r))\n")
      Seq(file)
    },
  )
  .dependsOn(core)

// Compiled with the oldest Scala version find-unused supports, so it runs in each newer compiler
lazy val compilerPlugin = baseProj("compiler-plugin", "find-unused-compiler-plugin", Seq(scala3ForSbt))
  .settings(publishSettings)
  .settings(libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Provided)

lazy val cliClasspath = taskKey[Seq[File]]("CLI classpath")
lazy val compilerPluginJar = taskKey[File]("Compiler plugin jar")

def pluginSbtVersion(scalaBinaryVersion: String, sbt1Version: String): String =
  scalaBinaryVersion match {
    case "2.12" => sbt1Version
    case _ => "2.0.0"
  }

lazy val plugin = baseProj("plugin", "find-unused-plugin", Seq(scala2, scala3ForSbt))
  .settings(publishSettings)
  .settings(
    name := "find-unused-plugin",
    publishConfiguration := publishConfiguration.value.withOverwrite(scalaVersion.value == scala3ForSbt),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(scalaVersion.value == scala3ForSbt),
    pluginCrossBuild / sbtVersion := pluginSbtVersion(scalaBinaryVersion.value, "1.9.0"),
    scriptedBufferLog := false,
    scriptedLaunchOpts ++= Seq(s"-Dplugin.version=${version.value}", s"-Dscala.version=$scriptedScalaVersion"),
    scriptedSbt := pluginSbtVersion(scalaBinaryVersion.value, sbtVersion.value),
    libraryDependencies ++= Seq(
      // coursier publishes Scala 3 from 2.1.26 and Scala 2.12 only up to 2.1.24
      "io.get-coursier" %% "coursier" % (if (scalaBinaryVersion.value == "3") "2.1.26" else "2.1.24"),
    ) ++ (
      // scaladoc resolves the `@data` annotation that coursier's Scala 3 TASTy references; coursier doesn't depend on it
      if (scalaBinaryVersion.value == "3") Seq("io.github.alexarchambault" %% "data-class" % "0.2.11" % Provided)
      else Seq()
    ),
    cliClasspath := (cli.jvm(scala3ForLib) / Runtime / fullClasspath).value.map(_.data),
    compilerPluginJar := (compilerPlugin.jvm(scala3ForSbt) / Compile / packageBin).value,
    buildInfoKeys := Seq[BuildInfoKey](version, BuildInfoKey(cliClasspath), BuildInfoKey(compilerPluginJar)),
    buildInfoPackage := "bl.unused",
  )
  .enablePlugins(SbtPlugin, BuildInfoPlugin)
