// okay-cardano: Cardano as effects, on Scalus — package okay.cardano
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scalus = "1.4.0"

ThisBuild / organization := "io.github.okay-effects"
ThisBuild / homepage := Some(url("https://github.com/okay-effects/okay-cardano"))
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.3.8"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Wunused:imports,privates,locals")
ThisBuild / resolvers ++= Seq("okay-effects" at "https://okay-effects.github.io/okay-simple/maven",
  "okay-scala" at "https://okay-effects.github.io/okay-scala/maven",
  "okay-http" at "https://okay-effects.github.io/okay-http/maven")
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / versionScheme := Some("early-semver")
/** RELEASES, ON GITHUB PAGES: `sbt publish` writes both platforms as a Maven repository into `target/pages-maven/`,
 * which goes to the `gh-pages` branch under `maven/` (RELEASING.md) */
val pagesRepository = settingKey[File]("the Maven repository a release is written to, for GitHub Pages")
ThisBuild / pagesRepository := (ThisBuild / baseDirectory).value / "target" / "pages-maven"
ThisBuild / publishTo := Some(MavenCache("pages", (ThisBuild / pagesRepository).value))
ThisBuild / Compile / packageDoc / publishArtifact := false

lazy val cardano = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure).in(file("cardano"))
  .settings(name := "okay-cardano",
    libraryDependencies ++= Seq("io.github.okay-effects" %%% "okay-scala" % "0.1.1",
      "io.github.okay-effects" %%% "okay-http" % "0.1.1",
      "org.scalus" %%% "scalus" % scalus, "org.scalus" %%% "scalus-cardano-ledger" % scalus,
      "org.scalameta" %%% "munit" % "1.2.3" % Test))
  // Scalus on Scala.js imports @noble/hashes and @noble/curves (npm install): ES modules
  .jsSettings(scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)))

// FOR A PROJECT'S TESTS (the JVM): its contracts as a debugger's workspace, a dev server, scenarios verified, links
lazy val dev = (project in file("dev")).dependsOn(cardano.jvm)
  .settings(name := "okay-cardano-dev", libraryDependencies += "org.scalameta" %% "munit" % "1.2.3" % Test,
    // its tests compile a validator of their own
    addCompilerPlugin(("org.scalus" % "scalus-plugin" % scalus).cross(CrossVersion.full)))

lazy val root = (project in file(".")).aggregate(cardano.jvm, cardano.js, dev).settings(publish / skip := true)
