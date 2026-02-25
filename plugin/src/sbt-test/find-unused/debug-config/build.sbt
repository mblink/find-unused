Global / findUnusedUseLocalClasspath := true
Global / findUnusedUseRootDirectory := false
Global / findUnusedPackages += "bl.unused"

def quote(s: String): String = s""""$s""""
def jsonObj(fields: (String, String)*): String = "{" ++ fields.map { case (k, v) => s"""${quote(k)}:$v""" }.mkString(",") ++ "}"
def jsonArr(values: String*): String = "[" ++ values.mkString(",") ++ "]"

@transient lazy val check = taskKey[Unit]("check")

lazy val root = project.in(file(".")).settings(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "3.8.2",
  check := {
    val classpath = findUnusedFullTestClasspath.value
    val expected = jsonObj(
      "version" -> quote("0.2.0"),
      "configurations" -> jsonArr(jsonObj(
        "type" -> quote("scala"),
        "name" -> quote("Debug"),
        "request" -> quote("launch"),
        "mainClass" -> quote("bl.unused.FindUnusedCli"),
        "jvmOptions" -> jsonArr(),
        "args" -> jsonArr((Seq(
          "all",
          "--width", "0",
          "--package", "bl.unused",
        ) ++ classpath.flatMap(p => Seq("--classpath", p))).map(quote)*),
      )),
    )
    val actual = findUnusedAllDebugConfig.value

    if (expected != actual)
      sys.error(s"expected: $expected\n\nactual: $actual")
  }
)
