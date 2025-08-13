package bl.unused

import sbt.*
import sbt.Keys.*

private[unused] trait FindUnusedPluginCompat {
  final class cacheLevel(@annotation.nowarn("msg=never used") include: Array[Nothing]) extends annotation.StaticAnnotation

  final lazy val findUnusedFullTestClasspathTask = Def.task {
    (Test / fullClasspath).value.map(_.data.toString)
  }
}
