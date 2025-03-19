package bl.unused

import sbt.*
import sbt.Keys.*

private[unused] trait FindUnusedPluginCompat {
  final lazy val findUnusedFullTestClasspathTask = Def.task {
    val converter = fileConverter.value
    (Test / fullClasspath).value.map(f => converter.toPath(f.data).toString)
  }
}
