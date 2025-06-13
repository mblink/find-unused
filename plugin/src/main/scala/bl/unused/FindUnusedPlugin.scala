package bl.unused

import coursier.{Fetch, LocalRepositories, Repositories}
import coursier.core.{Dependency, Module, ModuleName, Organization}
import coursier.maven.MavenRepository
import java.io.{FileOutputStream, PrintWriter, StringWriter}
import sbt.*
import sbt.internal.util.complete.*
import sbt.internal.util.complete.Parsers.*
import sbt.Keys.*
import sbt.plugins.JvmPlugin
import scala.util.matching.Regex
import sjsonnew.support.scalajson.unsafe.CompactPrinter
import sjsonnew.shaded.scalajson.ast.unsafe.{JArray, JField, JString, JObject, JValue}

object FindUnusedPlugin extends AutoPlugin with FindUnusedPluginCompat {
  object autoImport {
    val findUnused = settingKey[Unit]("Find unused")

    val findUnusedCliDownload = taskKey[Seq[File]]("Find unused CLI download")

    val findUnusedCliClasspath = taskKey[Seq[File]]("Find unused CLI classpath")

    val findUnusedCommandOptions = taskKey[(ForkOptions, Seq[String], String => Seq[String])]("Find unused command options")

    val findUnusedUseLocalClasspath = settingKey[Boolean]("Whether to use the local classpath for find-unused")

    val findUnusedUseRootDirectory = settingKey[Boolean]("Whether to use the root directory for find-unused")

    val findUnusedOutputFile = settingKey[Option[File]]("A file to write find-unused output to instead of stdout")

    val findUnusedProjectsKey: AttributeKey[Seq[ProjectRef]] = AttributeKey("findUnusedProjectRefs")

    val findUnusedFullTestClasspath = taskKey[Seq[String]]("Find unused full test classpath")

    val findUnusedClasspathKey: AttributeKey[Seq[String]] = AttributeKey("findUnusedClasspath")

    val findUnusedStoreProjectClasspaths =
      inputKey[StateTransform]("Store the full classpaths of all projects for a given Scala version")

    val findUnusedDebug = settingKey[Boolean]("Whether to enable debug output for find-unused")

    val findUnusedPackages = settingKey[Seq[String]]("Packages to analyze with find unused")

    val findUnusedOnFail = taskKey[Int => Unit]("Callback to run when any find-unused command fails")

    case class FindUnusedExclusion(
      src: Option[Regex],
      sym: Option[Regex],
    ) {
      final lazy val toArg: String = List(
        "src" -> src,
        "sym" -> sym,
      ).flatMap { case (k, vo) => vo.map(v => s"$k=$v") }.mkString("&")
    }

    object FindUnusedExclusion {
      def apply(src: Regex): FindUnusedExclusion =
        new FindUnusedExclusion(src = Some(src), sym = None)

      def apply(sym: Regex)(implicit d: DummyImplicit): FindUnusedExclusion =
        new FindUnusedExclusion(src = None, sym = Some(sym))

      def apply(src: Regex, sym: Regex): FindUnusedExclusion =
        new FindUnusedExclusion(src = Some(src), sym = Some(sym))
    }

    val findUnusedExclusions = settingKey[Seq[FindUnusedExclusion]]("Find unused exclusions")

    val findUnusedExplicits = taskKey[Unit]("Find unused explicits")

    val findUnusedGivens = taskKey[Unit]("Find unused givens")

    val findUnusedImplicits = taskKey[Unit]("Find unused implicits")

    val findUnusedAll = taskKey[Unit]("Find all unused terms")

    val findUnusedExplicitsDebugConfig = taskKey[String]("Generate VS Code launch.json file to debug find-unused explicits")

    val findUnusedGivensDebugConfig = taskKey[String]("Generate VS Code launch.json file to debug find-unused givens")

    val findUnusedAllDebugConfig = taskKey[String]("Generate VS Code launch.json file to debug find-unused all")
  }

  import autoImport.*

  override lazy val trigger = allRequirements
  override lazy val requires = JvmPlugin

  private val findUnusedStoreAllProjectClasspaths = "findUnusedStoreAllProjectClasspaths"

  private lazy val commands = Seq(
    Command.command(findUnusedStoreAllProjectClasspaths)(findUnusedStoreAllProjectClasspaths),
  )

  override lazy val globalSettings: Seq[Setting[?]] = Def.settings(
    findUnused / javaOptions := Seq.empty,
    findUnusedCliDownload := findUnusedCliDownloadTask.value,
    findUnusedCliClasspath := findUnusedCliClasspathTask.value,
    findUnusedCommandOptions := findUnusedCommandOptionsTask.value,
    findUnusedUseLocalClasspath := false,
    findUnusedUseRootDirectory := true,
    findUnusedOutputFile := None,
    findUnusedStoreProjectClasspaths := findUnusedStoreProjectClasspathsTask.evaluated,
    findUnusedDebug := false,
    findUnusedPackages := Seq.empty,
    findUnusedOnFail := (code => throw new MessageOnlyException(s"find-unused failed (exit code $code)")),
    findUnusedExclusions := Seq.empty,
    findUnusedExplicits := findUnusedExplicitsTask.value,
    findUnusedGivens := findUnusedGivensTask.value,
    findUnusedImplicits := findUnusedGivensTask.value,
    findUnusedAll := findUnusedAllTask.value,
    findUnusedExplicitsDebugConfig := findUnusedExplicitsDebugConfigTask.value,
    findUnusedGivensDebugConfig := findUnusedGivensDebugConfigTask.value,
    findUnusedAllDebugConfig := findUnusedAllDebugConfigTask.value,
    Keys.commands ++= commands,
  )

  override lazy val projectSettings: Seq[Setting[?]] = Def.settings(
    findUnusedFullTestClasspath := findUnusedFullTestClasspathTask.value,
  )

  // https://stackoverflow.com/a/39868021/2163024
  private def autoClose[A <: AutoCloseable, B](a: A)(f: A => B): B = {
    var err: Throwable = null
    try { f(a) }
    catch {
      case t: Throwable =>
        err = t
        err.printStackTrace()
        throw err
    } finally {
      if (err != null)
        try { a.close() }
        catch {
          case t: Throwable =>
            err.addSuppressed(t)
            err.printStackTrace()
            throw err
        }
      else
        a.close()
    }
  }

  private def runLogged[A](log: Logger, f: () => A): A =
    scala.util.Try(f()).fold(
      e => {
        autoClose(new StringWriter())(sw =>
          autoClose(new PrintWriter(sw)) { pw =>
            e.printStackTrace(pw)
            log.error(sw.toString)
          }
        )
        throw e
      },
      identity,
    )

  private lazy val findUnusedCliDownloadTask = Def.task {
    val log = streams.value.log

    log.info("Fetching find-unused CLI with coursier")

    val dep = Dependency(Module(Organization("bondlink"), ModuleName("find-unused-cli_3"), Map.empty), BuildInfo.version)
    val repos = Seq(
      Repositories.central,
      LocalRepositories.ivy2Local,
      MavenRepository("https://maven.bondlink-cdn.com"),
    )
    runLogged(log, () => Fetch().withRepositories(repos).addDependencies(dep).run())
  }

  private lazy val findUnusedCliClasspathTask = Def.taskDyn {
    if (findUnusedUseLocalClasspath.value) Def.task[Seq[File]](BuildInfo.cliClasspath)
    else Def.task(findUnusedCliDownload.value)
  }

  private val scalaVersionParser = {
    val valid = Set('.', '-', '+')
    identifier(
      Parser.charClass(alphanum, "alphanum"),
      Parser.charClass(c => alphanum(c) || valid.contains(c), "version character")
    )
  }

  private lazy val findUnusedStoreProjectClasspathsTask = Def.inputTaskDyn {
    val scalaVersionInput = (Parsers.Space ~> scalaVersionParser).parsed
    val state = Keys.state.value
    val projectRefs = state.attributes(findUnusedProjectsKey).filter(p => state.setting(p / scalaVersion) == scalaVersionInput)

    Def.task {
      val classpath: Seq[String] = projectRefs
        .map(_ / findUnusedFullTestClasspath)
        .join
        .value
        .flatten
        .distinct

      StateTransform { s =>
        val oldClasspath = s.attributes(findUnusedClasspathKey)
        s.put(findUnusedClasspathKey, (oldClasspath ++ classpath).distinct)
      }
    }
  }

  private def findUnusedStoreAllProjectClasspaths(state: State): State = {
    val loadedBuild = state.setting(Keys.loadedBuild)
    // all project refs that have a Scala version
    val projectRefs = loadedBuild.allProjectRefs.map(_._1).filter(p => state.getSetting(p / scalaVersion).isDefined)
    // all cross scala versions of those projects
    val scalaVersions = projectRefs.flatMap(p => state.setting(p / crossScalaVersions)).distinct

    val initState = state
      .put(findUnusedClasspathKey, Seq.empty[String])
      .put(findUnusedProjectsKey, projectRefs)

    val storeAllClasspaths = scalaVersions.flatMap(v =>
      Seq(s"++$v", s"Global/${findUnusedStoreProjectClasspaths.key} $v")
    )

    storeAllClasspaths.toList ::: initState
  }

  private lazy val mainClass = "bl.unused.FindUnusedCli"

  private lazy val findUnusedCommandOptionsTask = Def.task {
    val log = streams.value.log
    val outputFile = findUnusedOutputFile.value
    val outputStrategy = outputFile.map(f => OutputStrategy.CustomOutput(new FileOutputStream(f)))
    val forkOpts = ForkOptions()
      .withJavaHome(javaHome.value)
      .withOutputStrategy(outputStrategy)
      .withConnectInput(false)
      .withRunJVMOptions((findUnused / javaOptions).value.toVector)

    val cliClasspath = findUnusedCliClasspath.value
    val classpathOpts = List("-cp", Path.makeString(cliClasspath))

    @annotation.tailrec
    def runCommand(command: String, state: State): State = {
      val nextState = Parser.parse(command, state.combinedParser).fold(
        msg => sys.error(s"Invalid command: $msg"),
        cmd => cmd()
      )
      nextState.remainingCommands.toList match {
        case Nil => nextState
        case h :: t => runCommand(h.commandLine, nextState.copy(remainingCommands = t))
      }
    }

    val state0 = Keys.state.value
    val state = runCommand(
      findUnusedStoreAllProjectClasspaths,
      state0.copy(remainingCommands = Nil)
    ).copy(remainingCommands = state0.remainingCommands)

    val projClasspath = state.attributes(findUnusedClasspathKey)
    val projPackages = findUnusedPackages.value

    val useRootDir = findUnusedUseRootDirectory.value
    val rootDir = (ThisBuild / baseDirectory).value.toString
    val termWidth = terminal.value.getWidth
    val debug = findUnusedDebug.value
    val exclusions = findUnusedExclusions.value

    (forkOpts, classpathOpts, (cmd: String) => {
      val javaOpts =
        Seq(cmd) ++
        (if (debug) Seq("--debug") else Seq.empty) ++
        (if (useRootDir) Seq("--root-directory", rootDir) else Seq.empty) ++
        outputFile.map(f => Seq("--output", f.toString)).getOrElse(Seq.empty) ++
        Seq("--width", termWidth.toString) ++
        projPackages.flatMap(Seq("--package", _)) ++
        projClasspath.flatMap(Seq("--classpath", _)) ++
        exclusions.flatMap(e => Seq("--exclusion", e.toArg))

      log.debug(s"Running find-unused with arguments: ${javaOpts.mkString(" ")}")

      javaOpts
    })
  }

  private def findUnusedTask(cmd: String) = Def.task {
    val (forkOpts, classpathOpts, javaOpts) = findUnusedCommandOptions.value
    val onFail = findUnusedOnFail.value

    Fork.java.fork(forkOpts, classpathOpts ++ Seq(mainClass) ++ javaOpts(cmd)).exitValue() match {
      case 0 => ()
      case code => onFail(code)
    }
  }

  private lazy val findUnusedExplicitsTask = findUnusedTask("explicits")
  private lazy val findUnusedGivensTask = findUnusedTask("givens")
  private lazy val findUnusedAllTask = findUnusedTask("all")

  private def findUnusedDebugConfigTask(cmd: String) = Def.task {
    val log = streams.value.log
    val (forkOpts, _, javaOpts) = findUnusedCommandOptions.value
    val conf = CompactPrinter(JObject(Array(
      JField("version", JString("0.2.0")),
      JField("configurations", JArray(JObject(Array(
        JField("type", JString("scala")),
        JField("name", JString("Debug")),
        JField("request", JString("launch")),
        JField("mainClass", JString(mainClass)),
        JField("jvmOptions", JArray(forkOpts.runJVMOptions.map(JString(_): JValue).toArray)),
        JField("args", JArray(javaOpts(cmd).map(JString(_): JValue).toArray)),
      )))),
    )))
    log.info(conf)
    conf
  }

  private lazy val findUnusedExplicitsDebugConfigTask = findUnusedDebugConfigTask("explicits")
  private lazy val findUnusedGivensDebugConfigTask = findUnusedDebugConfigTask("givens")
  private lazy val findUnusedAllDebugConfigTask = findUnusedDebugConfigTask("all")
}
