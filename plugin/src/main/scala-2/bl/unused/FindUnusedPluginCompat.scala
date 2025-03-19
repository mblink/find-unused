package bl.unused

import sbt.*
import sbt.Keys.*

private[unused] trait FindUnusedPluginCompat {
  final lazy val findUnusedFullTestClasspathTask = Def.task {
    (Test / fullClasspath).value.map(_.data.toString)
  }
}
