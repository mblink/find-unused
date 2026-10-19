package bl.unused

import coursier.core.{Dependency, Module}
import sbt.*
import sbt.Keys.*

private[unused] trait FindUnusedPluginCompat {
  final class cacheLevel(@annotation.nowarn("msg=never used") include: Array[Nothing]) extends annotation.StaticAnnotation

  final def coursierDependency(module: Module, version: String): Dependency = Dependency(module, version)

  final lazy val findUnusedFullTestClasspathTask = Def.task {
    (Test / fullClasspath).value.map(_.data.toString)
  }
}
