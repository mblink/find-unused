package bondlink

import coursier.Fetch
import coursier.core.*
import coursier.maven.MavenRepository
import dev.dirs.BaseDirectories
import java.io.FileOutputStream
import java.net.URI
import java.nio.channels.Channels
import sbt.*
import sbt.internal.util.complete.*
import sbt.internal.util.complete.Parsers.*
import sbt.Keys.*
import sbt.plugins.JvmPlugin
import scala.sys.process.*

object FindUnusedPlugin extends AutoPlugin {
  sealed trait CliRunnable
  case class CliBinary(path: File) extends CliRunnable
  case class CliClasspath(cp: Seq[File]) extends CliRunnable

  object autoImport {
    val findUnused = settingKey[Unit]("Find unused")

    val findUnusedCliBinaryDownload = taskKey[CliRunnable]("Find unused CLI binary download")

    val findUnusedCliCoursierDownload = taskKey[CliClasspath]("Find unused CLI coursier download")

    val findUnusedCliRunnable = taskKey[CliRunnable]("Find unused CLI runnable")

    val findUnusedCommandOptions = taskKey[(CliRunnable, Seq[String])]("Find unused command options")

    val findUnusedUseCoursier = settingKey[Boolean]("Find unused use coursier")

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
    findUnusedCliBinaryDownload := findUnusedCliBinaryDownloadTask.value,
    findUnusedCliCoursierDownload := findUnusedCliCoursierDownloadTask.value,
    findUnusedCliRunnable := findUnusedCliRunnableTask.value,
    findUnusedCommandOptions := findUnusedCommandOptionsTask.value,
    findUnusedUseCoursier := false,
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
        throw t
    } finally {
      if (err != null)
        try { a.close() }
        catch {
          case t: Throwable =>
            err.addSuppressed(t)
            throw t
        }
      else
        a.close()
    }
  }

  private def sysProp(name: String): Option[String] = Option(System.getProperty(name)).map(_.toLowerCase)

  private def isLinux(os: String): Boolean = os.contains("linux")
  private def isMac(os: String): Boolean = os.contains("mac")
  private def isWindows(os: String): Boolean = os.contains("windows")

  private def isARM(arch: String): Boolean = arch.contains("aarch64") || arch.contains("arm64")
  private def isX64(arch: String): Boolean = arch.contains("x86_64") || arch.contains("amd64")

  private def coursierDownload(log: Logger): CliClasspath = {
    log.info("Fetching find-unused CLI with coursier")
    val dep = Dependency(Module(Organization("bondlink"), ModuleName("find-unused-cli_3"), Map.empty), BuildInfo.version)
    val repos = Seq(MavenRepository("https://raw.githubusercontent.com/mblink/maven-repo/main"))
    val cp = Fetch().withRepositories(repos).addDependencies(dep).run()
    CliClasspath(cp)
  }

  private lazy val findUnusedCliBinaryDownloadTask = Def.task {
    val log = streams.value.log
    val cliBinaryName = (sysProp("os.name"), sysProp("os.arch")) match {
      case (Some(os), Some(arch)) if isLinux(os) && isARM(arch) => Some("find-unused-linux-arm")
      case (Some(os), Some(arch)) if isLinux(os) && isX64(arch) => Some("find-unused-linux")
      case (Some(os), Some(arch)) if isMac(os) && isARM(arch) => Some("find-unused-mac-arm")
      case (Some(os), Some(arch)) if isMac(os) && isX64(arch) => Some("find-unused-mac")
      case (Some(os), _) if isWindows(os) => Some("find-unused-windows.exe")
      case _ => None
    }

    cliBinaryName match {
      case Some(bin) =>
        val binDir = file(BaseDirectories.get.cacheDir) / "find-unused" / BuildInfo.version
        val binPath = binDir / bin
        val binPathTgz = binDir / s"$bin.tar.gz"

        log.info(s"Checking for binary $binPath")

        if (!binPath.exists) {
          val url = s"https://github.com/mblink/find-unused/releases/download/v${BuildInfo.version}/$bin.tar.gz"
          log.info(s"Binary $binPath does not exist, downloading from $url...")

          binDir.mkdirs

          autoClose(Channels.newChannel(new URI(url).toURL.openStream))(channel =>
            autoClose(new FileOutputStream(binPathTgz))(output =>
              output.getChannel.transferFrom(channel, 0, Long.MaxValue)
            )
          )

          log.info(s"Extracting $binPathTgz to $binPath")

          Seq("tar", "-xzf", s"$bin.tar.gz").! match {
            case 0 => ()
            case code => throw new MessageOnlyException(s"Failed to extract binary (exit code $code)")
          }
        }

        CliBinary(binPath)

      case None =>
        coursierDownload(log)
    }
  }

  private lazy val findUnusedCliCoursierDownloadTask = Def.task {
    coursierDownload(streams.value.log)
  }

  private lazy val findUnusedCliRunnableTask = Def.taskDyn {
    if (findUnusedUseLocalClasspath.value) Def.task[CliRunnable](CliClasspath(BuildInfo.cliClasspath))
    else if (findUnusedUseCoursier.value) Def.task[CliRunnable](findUnusedCliCoursierDownload.value)
    else Def.task(findUnusedCliBinaryDownload.value)
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
    val cliRunnable = findUnusedCliRunnable.value

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

    val cliOpts = projClasspath.flatMap(Seq("-c", _)) ++ projPackages.flatMap(Seq("-p", _))

    (cliRunnable, cliOpts)
  }

  private lazy val findUnusedGivensTask = Def.task {
    val exitCode = findUnusedCommandOptions.value match {
      case (CliBinary(cliBinary), cliOpts) =>
        (cliBinary.toString +: cliOpts).!

      case (CliClasspath(cliClasspath), cliOpts) =>
        val forkOpts = ForkOptions()
          .withJavaHome(javaHome.value)
          .withOutputStrategy(outputStrategy.value)
          .withConnectInput(false)
          .withRunJVMOptions((findUnused / javaOptions).value.toVector)

        val scalaOpts = List("-classpath", Path.makeString(cliClasspath), "bondlink.FindUnusedCli")

        Fork.java.fork(forkOpts, scalaOpts ++ List("givens") ++ cliOpts).exitValue()
    }

    exitCode match {
      case 0 => ()
      case 1 => throw new MessageOnlyException("findUnusedGivens failed, see output above")
    }
  }
}
