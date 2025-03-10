package bondlink

import coursier.Fetch
import coursier.core.*
import coursier.maven.MavenRepository
import java.io.{PrintWriter, StringWriter}
import sbt.*
import sbt.internal.util.complete.*
import sbt.internal.util.complete.Parsers.*
import sbt.Keys.*
import sbt.plugins.JvmPlugin

object FindUnusedPlugin extends AutoPlugin {
  object autoImport {
    val findUnused = settingKey[Unit]("Find unused")

    val findUnusedCliDownload = taskKey[Seq[File]]("Find unused CLI download")

    val findUnusedCliClasspath = taskKey[Seq[File]]("Find unused CLI classpath")

    val findUnusedCommandOptions = taskKey[(ForkOptions, String => Seq[String])]("Find unused command options")

    val findUnusedUseLocalClasspath = settingKey[Boolean]("Find unused use local classpath")

    val findUnusedProjectsKey: AttributeKey[Seq[ProjectRef]] = AttributeKey("findUnusedProjectRefs")

    val findUnusedClasspathKey: AttributeKey[Seq[String]] = AttributeKey("findUnusedClasspath")

    val findUnusedStoreProjectClasspaths =
      inputKey[StateTransform]("Store the full classpaths of all projects for a given Scala version")

    val findUnusedPackages = taskKey[Seq[String]]("Packages to analyze with find unused")

    val findUnusedGivens = taskKey[Unit]("Find unused givens")
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
    findUnusedStoreProjectClasspaths := findUnusedStoreProjectClasspathsTask.evaluated,
    findUnusedPackages := Seq.empty,
    findUnusedGivens := findUnusedGivensTask.value,
    Keys.commands ++= commands,
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
    val repos = Seq(MavenRepository("https://raw.githubusercontent.com/mblink/maven-repo/main"))
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
        .map(p => (p / Test / fullClasspath))
        .join
        .value
        .flatMap(_.map(_.data.toString))
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

  private lazy val findUnusedCommandOptionsTask = Def.task {
    val log = streams.value.log
    val forkOpts = ForkOptions()
      .withJavaHome(javaHome.value)
      .withOutputStrategy(outputStrategy.value)
      .withConnectInput(false)
      .withRunJVMOptions((findUnused / javaOptions).value.toVector)

    val cliClasspath = findUnusedCliClasspath.value
    val mainClass = "bondlink.FindUnusedCli"
    val baseJavaOpts = List("-cp", Path.makeString(cliClasspath), mainClass)

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

    val rootDir = (ThisBuild / baseDirectory).value.toString
    val termWidth = terminal.value.getWidth

    (forkOpts, (cmd: String) => {
      val javaOpts = baseJavaOpts ++
        Seq(
          cmd,
          "--root-directory",
          rootDir,
          "--width",
          termWidth.toString,
        ) ++
        projPackages.flatMap(Seq("--package", _)) ++
        projClasspath.flatMap(Seq("--classpath", _))

      log.debug(s"Running find-unused with arguments: ${javaOpts.mkString(" ")}")

      javaOpts
    })
  }

  private lazy val findUnusedGivensTask = Def.task {
    val (forkOpts, javaOpts) = findUnusedCommandOptions.value

    Fork.java.fork(forkOpts, javaOpts("givens")).exitValue() match {
      case 0 => ()
      case 1 => throw new MessageOnlyException("findUnusedGivens failed, see output above")
    }
  }
}
