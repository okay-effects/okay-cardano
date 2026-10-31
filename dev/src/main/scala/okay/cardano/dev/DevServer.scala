package okay.cardano.dev

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.net.InetSocketAddress
import java.nio.file.{Files, Path}
import okay.cardano.Scenario

/**
 * THE PROJECT FOR THE DEBUGGER'S PAGE, on this machine: `GET /workspace` (the workspace file, with an ETag so the page
 * sees when it changed), `GET /scenarios` (every scenario), `PUT` / `DELETE /scenarios/<name>.json`. Only the page's
 * origins are answered (CORS), and Chrome's Private Network Access is allowed, so the live page can reach localhost
 * once the person allows it. EVERYTHING ELSE is the page itself, fetched from `page`: opened from this server, the page
 * and its project share one origin and the browser asks nothing.
 * `sbt "Test/runMain okay.cardano.dev.DevServer"`, then open the address it prints.
 */
final class DevServer(root: Path, port: Int, origins: String => Boolean, page: String = OkayUplc.page):
  private val workspace = root.resolve(Workspace.file)
  private val scenarios = root.resolve(Scenarios.dir)
  private val upstream = java.net.http.HttpClient.newBuilder()
    .followRedirects(java.net.http.HttpClient.Redirect.NORMAL).build()
  private val server = HttpServer.create(new InetSocketAddress("127.0.0.1", port), 0)

  def address: String = s"http://127.0.0.1:${server.getAddress.getPort}"
  def start(): DevServer = { server.start(); this }
  def stop(): Unit = server.stop(0)

  server.createContext("/", (x: HttpExchange) => scala.util.Try(answer(x)).recover { case e =>
    send(x, 500, "text/plain", Option(e.getMessage).getOrElse(e.toString).getBytes("UTF-8"))
  })

  private def cors(x: HttpExchange): Boolean =
    val origin = Option(x.getRequestHeaders.getFirst("Origin"))
    origin.foreach { o =>
      if origins(o) then
        val h = x.getResponseHeaders
        h.set("Access-Control-Allow-Origin", o)
        h.set("Access-Control-Allow-Methods", "GET, PUT, DELETE, OPTIONS")
        h.set("Access-Control-Allow-Headers", "content-type, if-none-match")
        h.set("Access-Control-Expose-Headers", "etag")
        h.set("Access-Control-Allow-Private-Network", "true")
        h.set("Vary", "Origin")
    }
    origin.forall(origins)

  private def send(x: HttpExchange, status: Int, kind: String, body: Array[Byte]): Unit =
    if body.nonEmpty then x.getResponseHeaders.set("content-type", kind)
    x.sendResponseHeaders(status, if body.isEmpty then -1 else body.length)
    if body.nonEmpty then x.getResponseBody.write(body)
    x.close()

  private def name(path: String): Option[String] =
    Some(path.stripPrefix("/scenarios/")).filter(n => n.matches("[A-Za-z0-9._-]+\\.json") && !n.startsWith("."))

  private def answer(x: HttpExchange): Unit =
    val allowed = cors(x)
    val path = x.getRequestURI.getPath
    if !allowed then send(x, 403, "text/plain", "not an origin of the debugger's page".getBytes)
    else if x.getRequestMethod == "OPTIONS" then send(x, 204, "", Array.empty)
    else (x.getRequestMethod, path) match
      case ("GET", "/workspace") =>
        if !Files.exists(workspace) then send(x, 404, "text/plain", s"no $workspace yet: Workspace.write(…)".getBytes)
        else
          val etag = s"\"${Files.getLastModifiedTime(workspace).toMillis}-${Files.size(workspace)}\""
          x.getResponseHeaders.set("etag", etag)
          if Option(x.getRequestHeaders.getFirst("If-None-Match")).contains(etag) then send(x, 304, "", Array.empty)
          else send(x, 200, "application/json", Files.readAllBytes(workspace))
      case ("GET", "/scenarios") =>
        val all = Scenarios.files(scenarios).map(f => ujson.Obj("file" -> f.getFileName.toString,
          "text" -> Files.readString(f)))
        send(x, 200, "application/json", ujson.Arr.from(all).render().getBytes("UTF-8"))
      case ("PUT", p) if name(p).isDefined =>
        val text = String(x.getRequestBody.readAllBytes(), "UTF-8")
        Scenario.read(text) match
          case Left(why) => send(x, 400, "text/plain", why.getBytes("UTF-8"))
          case Right(_) =>
            Files.createDirectories(scenarios)
            Files.writeString(scenarios.resolve(name(p).get), text)
            send(x, 204, "", Array.empty)
      case ("DELETE", p) if name(p).isDefined =>
        Files.deleteIfExists(scenarios.resolve(name(p).get))
        send(x, 204, "", Array.empty)
      case ("GET" | "HEAD" | "POST", p) if !p.startsWith("/workspace") && !p.startsWith("/scenarios") => relay(x)
      case _ => send(x, 404, "text/plain", s"nothing at $path".getBytes)

  /** the page's own files and its edge (`/koios/...`), asked of the page where it is deployed */
  private def relay(x: HttpExchange): Unit =
    import java.net.http.{HttpRequest, HttpResponse}
    val uri = x.getRequestURI
    val target = java.net.URI.create(page.stripSuffix("/") + uri.getRawPath +
      Option(uri.getRawQuery).fold("")("?" + _))
    val body = x.getRequestBody.readAllBytes()
    val b = HttpRequest.newBuilder(target).method(x.getRequestMethod,
      if body.isEmpty then HttpRequest.BodyPublishers.noBody() else HttpRequest.BodyPublishers.ofByteArray(body))
    Option(x.getRequestHeaders.getFirst("content-type")).foreach(b.header("content-type", _))
    val r = upstream.send(b.build(), HttpResponse.BodyHandlers.ofByteArray())
    val kind = r.headers.firstValue("content-type").orElse("application/octet-stream")
    if x.getRequestMethod == "HEAD" then
      x.getResponseHeaders.set("content-type", kind)
      x.sendResponseHeaders(r.statusCode, -1)
      x.close()
    else send(x, r.statusCode, kind, r.body)

object DevServer:
  /** the debugger's page, and a page served from this machine (a local copy, its tests) */
  def origins(o: String): Boolean =
    o == "https://okay-uplc.okay-effects.workers.dev" || o.matches("http://(localhost|127\\.0\\.0\\.1)(:\\d+)?")

  def main(args: Array[String]): Unit =
    val port = args.sliding(2).collectFirst { case Array("--port", p) => p.toInt }.getOrElse(8790)
    val server = DevServer(Path.of("").toAbsolutePath, port, origins).start()
    println(s"okay-uplc workspace on ${server.address}")
    println(s"open ${server.address}/?workspace=${server.address}")
    println(s"  (or ${OkayUplc.page}?workspace=${server.address}, once the browser may reach this machine)")
    Thread.currentThread().join()
