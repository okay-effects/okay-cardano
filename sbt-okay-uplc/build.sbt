// THE SBT PLUGIN: a Scalus project's contracts in okay-uplc with no code of its own. Built apart from the library
// (an sbt 1 plugin is Scala 2.12); released into the same Maven repository on GitHub Pages (../target/pages-maven).
ThisBuild / organization := "io.github.okay-effects"
ThisBuild / version := "0.1.0"
ThisBuild / homepage := Some(url("https://github.com/okay-effects/okay-cardano"))
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "sbt-okay-uplc",
    publishTo := Some(MavenCache("pages", baseDirectory.value / ".." / "target" / "pages-maven")),
    Compile / packageDoc / publishArtifact := false,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    scriptedBufferLog := false
  )
