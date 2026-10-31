package okay.cardano.dev

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import okay.cardano.{Blueprints, CaseFile, Scenario}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** A project's workspace written, its scenarios verified, served to the page, and a run as a link */
class DevSuite extends munit.FunSuite:
  private val owner = "07" * 28
  private def scenario(signer: String, passes: Boolean) = Scenario(s"signed by ${signer.take(4)}", "Gate",
    datum = Some(ByteString.fromHex(owner).toData), transaction = Scenario.Transaction(signatories = List(signer)),
    expect = Scenario.Expect(passes = passes, message = Option.when(!passes)("the owner must sign")))

  test("the workspace: a blueprint whose code is the debug build, with its Scala source map"):
    val dir = Files.createTempDirectory("ws")
    Workspace.writeTo(dir.resolve("workspace.json"), "gate", List(GateContract))
    val text = Files.readString(dir.resolve("workspace.json"))
    val read = Blueprints.read(text).fold(fail(_), identity)
    assertEquals(read.validators.map(_.title), List("Gate"))
    val release = ujson.read(GateContract.blueprint.toJson())("validators")(0)("compiledCode").str
    assertNotEquals(read.validators.head.compiledCode, release)
    val sources = ujson.read(text)("validators")(0)("okay-uplc")("sources")
    assert(sources("lines").obj.keys.exists(_.endsWith("Gate.scala")), sources.render().take(300))
    val sir = okay.cardano.Builds.decode(ujson.read(text)("validators")(0)("okay-uplc")("sir").str)
    assertEquals(okay.cardano.Builds.compile(sir, Workspace.compiled(GateContract).options).flatEncoded.toList,
      Workspace.compiled(GateContract).program.deBruijnedProgram.flatEncoded.toList)

  test("scenarios verified: as they expect, or the reasons"):
    val dir = Files.createTempDirectory("sc")
    Files.writeString(dir.resolve("owner.json"), Scenario.write(scenario(owner, passes = true)))
    Files.writeString(dir.resolve("stranger.json"), Scenario.write(scenario("08" * 28, passes = false)))
    assertEquals(Scenarios.verifyIn(dir, List(GateContract)), Nil)
    Files.writeString(dir.resolve("wrong.json"), Scenario.write(scenario("08" * 28, passes = true)))
    val wrong = Scenarios.verifyIn(dir, List(GateContract))
    assertEquals(wrong.map(_._1), List("wrong.json"))
    assert(wrong.head._2.last.startsWith("step through it: https://okay-uplc.okay-effects.workers.dev/#case="))

  test("the dev server: the workspace with its ETag, scenarios read and written, only the page's origins"):
    val root = Files.createTempDirectory("proj")
    Workspace.writeTo(root.resolve(Workspace.file), "gate", List(GateContract))
    val server = DevServer(root, 0, DevServer.origins).start()
    val http = HttpClient.newHttpClient()
    def call(method: String, path: String, body: String = "", headers: Map[String, String] = Map.empty) =
      val b = HttpRequest.newBuilder(URI.create(server.address + path))
        .method(method, if body.isEmpty then HttpRequest.BodyPublishers.noBody()
          else HttpRequest.BodyPublishers.ofString(body))
      (headers + ("Origin" -> "https://okay-uplc.okay-effects.workers.dev")).foreach((k, v) => b.header(k, v))
      http.send(b.build(), HttpResponse.BodyHandlers.ofString())
    try
      val ws = call("GET", "/workspace")
      assertEquals(ws.statusCode, 200)
      val etag = ws.headers.firstValue("etag").get
      val allowed = ws.headers.firstValue("access-control-allow-origin").get
      assertEquals(allowed, "https://okay-uplc.okay-effects.workers.dev")
      assertEquals(call("GET", "/workspace", headers = Map("If-None-Match" -> etag)).statusCode, 304)
      val pre = call("OPTIONS", "/scenarios/a.json", headers = Map("Access-Control-Request-Private-Network" -> "true"))
      assertEquals(pre.headers.firstValue("access-control-allow-private-network").get, "true")
      assertEquals(call("PUT", "/scenarios/owner.json", Scenario.write(scenario(owner, true))).statusCode, 204)
      assertEquals(call("PUT", "/scenarios/bad.json", "{}").statusCode, 400)
      assertEquals(call("PUT", "/scenarios/..%2Fescape.json", "{}").statusCode, 404)
      val listed = ujson.read(call("GET", "/scenarios").body).arr.map(_("file").str).toList
      assertEquals(listed, List("owner.json"))
      assert(Files.exists(root.resolve("okay-uplc/scenarios/owner.json")))
      assertEquals(call("DELETE", "/scenarios/owner.json").statusCode, 204)
      val stranger = http.send(HttpRequest.newBuilder(URI.create(server.address + "/workspace"))
        .header("Origin", "https://evil.example").build(), HttpResponse.BodyHandlers.ofString())
      assertEquals(stranger.statusCode, 403)
    finally server.stop()

  test("the dev server serves the page itself: whatever is not the project is asked of the page"):
    val page = com.sun.net.httpserver.HttpServer.create(java.net.InetSocketAddress("127.0.0.1", 0), 0)
    page.createContext("/", x =>
      val said = s"${x.getRequestMethod} ${x.getRequestURI}".getBytes
      x.getResponseHeaders.set("content-type", "text/html")
      x.sendResponseHeaders(200, said.length)
      x.getResponseBody.write(said)
      x.close())
    page.start()
    val root = Files.createTempDirectory("proj")
    val server = DevServer(root, 0, DevServer.origins, s"http://127.0.0.1:${page.getAddress.getPort}/").start()
    val http = HttpClient.newHttpClient()
    try
      val index = http.send(HttpRequest.newBuilder(URI.create(server.address + "/?workspace=x")).build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals((index.statusCode, index.body), (200, "GET /?workspace=x"))
      assertEquals(index.headers.firstValue("content-type").get, "text/html")
      val koios = http.send(HttpRequest.newBuilder(URI.create(server.address + "/koios/preview/tx_cbor"))
        .POST(HttpRequest.BodyPublishers.ofString("{}")).build(), HttpResponse.BodyHandlers.ofString())
      assertEquals(koios.body, "POST /koios/preview/tx_cbor")
      val none = http.send(HttpRequest.newBuilder(URI.create(server.address + "/workspace")).build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals(none.statusCode, 404)
    finally { server.stop(); page.stop(0) }

  test("the project's contracts found on the class path, or named"):
    assertEquals(Contracts.found().map(_.getClass.getName), List("okay.cardano.dev.GateContract$"))
    assertEquals(Contracts.named(List("okay.cardano.dev.GateContract")), List(GateContract))
    val dir = Path.of(classOf[GateContract.type].getProtectionDomain.getCodeSource.getLocation.toURI)
    assertEquals(Contracts.of(List("--classes", dir.toString)), List(GateContract))

  test("a contract compiled again from its SIR: the same program as Scalus's, other builds other programs"):
    val compiled = Workspace.compiled(GateContract)
    val sir = okay.cardano.Builds.decode(okay.cardano.Builds.encode(compiled.sir))
    assertEquals(okay.cardano.Builds.compile(sir, compiled.options).flatEncoded.toList,
      compiled.program.deBruijnedProgram.flatEncoded.toList)
    val debug = okay.cardano.Builds.compile(sir, compiled.withErrorTraces.options)
    assertEquals(debug.flatEncoded.toList, Workspace.debug(GateContract).flatEncoded.toList)
    val sizes = okay.cardano.Builds.presets.map(b => okay.cardano.Builds.compile(sir, b.options).flatEncoded.length)
    assert(sizes.distinct.size > 2, sizes)

  test("a run as a link: its fragment inflates to the case and the step"):
    val program = Workspace.debug(GateContract)
    val s = scenario("08" * 28, passes = false)
    val link = OkayUplc.link("a stranger", program, s.context(ByteString.fromArray(Array.fill(28)(0))))
    assert(link.startsWith("https://okay-uplc.okay-effects.workers.dev/#case="))
    val bytes = java.util.Base64.getUrlDecoder.decode(link.substring(link.indexOf("#case=") + 6))
    val inflater = java.util.zip.Inflater(true)
    inflater.setInput(bytes)
    val out = new Array[Byte](1 << 20)
    val n = inflater.inflate(out)
    val payload = ujson.read(String(out, 0, n, "UTF-8"))
    assertEquals(payload("at").num.toInt, -1)
    assertEquals(CaseFile.read(payload("file").str).map(_.name), Right("a stranger"))
