package okay.cardano

/** A PROJECT'S SOURCE FILES, on the JVM: a position's file read from the directory a test runs in (its root) */
object ProjectFiles:
  def read(file: String): Option[Vector[String]] =
    Option(java.nio.file.Path.of(file)).filter(java.nio.file.Files.isRegularFile(_))
      .map(p => scala.io.Source.fromFile(p.toFile, "UTF-8").getLines().toVector)
