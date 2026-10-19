package bl.unused

import coursier.core.{Dependency, Module}
import coursier.version.VersionConstraint
import sbt.*
import sbt.Keys.*

private[unused] trait FindUnusedPluginCompat {
  export sbt.util.cacheLevel

  final def coursierDependency(module: Module, version: String): Dependency = Dependency(module, VersionConstraint(version))

  final lazy val findUnusedFullTestClasspathTask = Def.task {
    val converter = fileConverter.value
    (Test / fullClasspath).value.map(f => converter.toPath(f.data).toString)
  }
}
