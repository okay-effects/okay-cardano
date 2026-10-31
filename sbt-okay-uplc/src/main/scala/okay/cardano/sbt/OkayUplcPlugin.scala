package okay.cardano.sbt

import sbt._
import sbt.Keys._

/** A SCALUS PROJECT IN OKAY-UPLC, by one line in `project/plugins.sbt`:
  * {{{
  * resolvers += "okay-cardano" at "https://okay-effects.github.io/okay-cardano/maven"
  * addSbtPlugin("io.github.okay-effects" % "sbt-okay-uplc" % "0.1.0")
  * }}}
  * It puts okay-cardano-dev on the test class path and gives:
  *   - `okayUplcWorkspace` — the contracts (every Scalus `Contract` object, or `okayUplcContracts`) written as the
  *     debugger's workspace, `target/okay-uplc/workspace.json`; `~okayUplcWorkspace` keeps it fresh;
  *   - `okayUplcVerify` — every scenario in `okay-uplc/scenarios` checked; a wrong one fails with its link;
  *   - `okayUplcServe` — the workspace written, then the dev server (http://127.0.0.1:8790, the page from it too);
  *     it runs until stopped; `okayUplcStart` the same in the background of an sbt shell (`jobStop` ends it).
  */
object OkayUplcPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val okayUplcVersion = settingKey[String]("the okay-cardano-dev release the project's tests use")
    val okayUplcContracts = settingKey[Seq[String]]("the contract objects, by name; empty: every one found")
    val okayUplcWorkspace = taskKey[Unit]("write okay-uplc's workspace: the contracts' debug builds and sources")
    val okayUplcVerify = taskKey[Unit]("check every scenario in okay-uplc/scenarios against what it expects")
    val okayUplcServe = taskKey[Unit]("write the workspace and serve it, and the page, on http://127.0.0.1:8790")
    val okayUplcStart = taskKey[Unit]("okayUplcServe in the background of an sbt shell")
  }
  import autoImport._


  /** where the project's own classes are: the contracts are looked for there */
  private val classes = Def.task {
    val dirs = Seq((Compile / classDirectory).value, (Test / classDirectory).value)
    "--classes " + dirs.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)
  }

  override def projectSettings: Seq[Setting[_]] = Seq(
    okayUplcVersion := "0.1.0",
    okayUplcContracts := Nil,
    resolvers ++= Seq(
      "okay-simple" at "https://okay-effects.github.io/okay-simple/maven",
      "okay-scala" at "https://okay-effects.github.io/okay-scala/maven",
      "okay-http" at "https://okay-effects.github.io/okay-http/maven",
      "okay-cardano" at "https://okay-effects.github.io/okay-cardano/maven"
    ),
    libraryDependencies += "io.github.okay-effects" %% "okay-cardano-dev" % okayUplcVersion.value % Test,
    okayUplcWorkspace := Def.taskDyn {
      val args = classes.value +: okayUplcContracts.value
      (Test / runMain).toTask((("okay.cardano.dev.Workspace") +: args).mkString(" ", " ", ""))
    }.value,
    okayUplcVerify := Def.taskDyn {
      val args = classes.value +: okayUplcContracts.value
      (Test / runMain).toTask((("okay.cardano.dev.Scenarios") +: args).mkString(" ", " ", ""))
    }.value,
    okayUplcServe := Def.sequential(okayUplcWorkspace, (Test / runMain).toTask(" okay.cardano.dev.DevServer")).value,
    okayUplcStart := Def.sequential(okayUplcWorkspace,
      (Test / bgRunMain).toTask(" okay.cardano.dev.DevServer").map(_ => ())).value
  )
}
