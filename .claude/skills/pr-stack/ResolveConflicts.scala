//> using dep com.lihaoyi::ujson:4.4.3
// Replaces the conflict blocks in a file, in order, with the given resolutions.
//
// Usage: scala-cli run ResolveConflicts.scala -- <file> <resolutions.json>
//
// <resolutions.json> is a list with one entry per conflict block: "ours", "theirs", or the literal text to put there
// (ending in a newline). It fails without changing the file if the counts differ. A diff3-style block's `|||||||` base
// section is dropped.
import java.nio.file.{Files, Paths}

private val conflictBlock =
  raw"(?ms)^<<<<<<< [^\n]*\n(.*?)(?:^\|\|\|\|\|\|\|[^\n]*\n.*?)?^=======\n(.*?)^>>>>>>> [^\n]*\n".r

@main def resolveConflicts(file: String, resolutionsFile: String): Unit = {
  val resolutions = ujson.read(Files.readString(Paths.get(resolutionsFile))).arr.map(_.str).toList
  val text = Files.readString(Paths.get(file))
  val blocks = conflictBlock.findAllMatchIn(text).toList
  if (blocks.size != resolutions.size) {
    System.err.println(s"$file: ${blocks.size} conflicts, but ${resolutions.size} resolutions")
    sys.exit(1)
  }
  val out = new StringBuilder
  var last = 0
  blocks.zip(resolutions).foreach { (block, resolution) =>
    out.append(text.substring(last, block.start))
    out.append(resolution match {
      case "ours" => block.group(1)
      case "theirs" => block.group(2)
      case other => other
    })
    last = block.end
  }
  out.append(text.substring(last))
  Files.writeString(Paths.get(file), out.toString): Unit
}
