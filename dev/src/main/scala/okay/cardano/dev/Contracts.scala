package okay.cardano.dev

import java.nio.file.{Files, Path}
import scalus.cardano.blueprint.Contract
import scala.jdk.CollectionConverters.*

/** A PROJECT'S CONTRACTS, FOUND: every Scala `object` extending Scalus's `Contract` in the class directories on the
  * class path (the project's own classes, not its libraries), or those named. What the mains of `Workspace` and
  * `Scenarios` run on, so a project needs no test of its own to call them.
  */
object Contracts:
  def named(names: Seq[String], loader: ClassLoader = Thread.currentThread.getContextClassLoader): List[Contract] =
    names.toList.map(n => module(Class.forName(n.stripSuffix("$") + "$", true, loader))
      .getOrElse(throw IllegalArgumentException(s"$n is not a Contract object")))

  def found(loader: ClassLoader = Thread.currentThread.getContextClassLoader, in: List[Path] = Nil): List[Contract] =
    (if in.nonEmpty then in else directories(loader)).filter(Files.isDirectory(_)).flatMap(classes).distinct.sorted
      .flatMap(n =>
      scala.util.Try(Class.forName(n, false, loader)).toOption
        .filter(classOf[Contract].isAssignableFrom).flatMap(module))

  /** `[--classes <dirs>] [names…]`: the given names, or else every contract found (in those class directories, as
    * sbt-okay-uplc passes them, or on the class path)
    */
  def of(args: Seq[String]): List[Contract] = args.toList match
    case "--classes" :: dirs :: rest =>
      if rest.nonEmpty then named(rest)
      else found(in = dirs.split(java.io.File.pathSeparator).filter(_.nonEmpty).map(Path.of(_)).toList)
    case names => if names.nonEmpty then named(names) else found()

  private def module(c: Class[?]): Option[Contract] =
    scala.util.Try(c.getField("MODULE$").get(null)).toOption.collect { case k: Contract => k }

  private def directories(loader: ClassLoader): List[Path] =
    val urls = Iterator.iterate(loader)(_.getParent).takeWhile(_ != null).flatMap {
      case u: java.net.URLClassLoader => u.getURLs.toList
      case _                          => Nil
    }.toList
    val paths = urls.map(u => Path.of(u.toURI)) ++
      sys.props.getOrElse("java.class.path", "").split(java.io.File.pathSeparator).filter(_.nonEmpty).map(Path.of(_))
    paths.filter(Files.isDirectory(_)).distinct

  private def classes(dir: Path): List[String] =
    Files.walk(dir).iterator.asScala.map(dir.relativize(_).toString).filter(_.endsWith("$.class"))
      .map(_.stripSuffix(".class").replace(java.io.File.separatorChar, '.')).toList
