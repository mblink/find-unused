package bl.unused

import sbt.*
import sbt.Keys.*

private[unused] trait FindUnusedPluginCompat extends sbt.util.CacheImplicits {
  export sbt.util.cacheLevel

  final lazy val findUnusedFullTestClasspathTask = Def.task {
    val converter = fileConverter.value
    (Test / fullClasspath).value.map(f => converter.toPath(f.data).toString)
  }
}
