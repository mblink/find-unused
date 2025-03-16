Global / findUnusedDebug := true
Global / findUnusedUseLocalClasspath := true
Global / findUnusedPackages += "bondlink"
// Global / findUnusedExclusions += FindUnusedExclusion(src = "src.*Test\\.scala$".r)

lazy val root = project.in(file("."))
  .settings(
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.6.3",
    resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "alleycats-core" % "2.13.0",
      "org.typelevel" %% "cats-core" % "2.13.0",
    ),
  )
