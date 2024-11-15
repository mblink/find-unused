Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project.in(file("."))
  .settings(
    name := "tasty-query-unused",
    organization := "bondlink",
    scalaVersion := "3.5.2",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "tasty-query" % "1.4.0",
      "com.lihaoyi" %% "os-lib" % "0.11.3",
      "org.typelevel" %% "cats-core" % "2.12.0",
    )
  )
