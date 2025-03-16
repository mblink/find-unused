package bl.unused

import java.nio.file.Path
import tastyquery.SourcePosition

object Positions {
  def format(rootDirectory: Option[Path], pos: SourcePosition): String =
    rootDirectory.fold("")(_.toString ++ "/") ++ (
      if (pos.hasLineColumnInformation) s"${pos.sourceFile}:${pos.pointLine + 1}:${pos.pointColumn + 1}"
      else pos.toString
    )
}
