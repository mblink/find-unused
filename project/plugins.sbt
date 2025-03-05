lazy val sbtGitHubActions = RootProject(uri(s"https://github.com/sbt/sbt-github-actions.git#8db8710ab7c69e452edd501d54eb4e12b3b2c03a"))
dependsOn(sbtGitHubActions)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.1.0")
// addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.24.0")
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.2")

resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
addSbtPlugin("bondlink" % "sbt-git-publish" % "0.0.5")
