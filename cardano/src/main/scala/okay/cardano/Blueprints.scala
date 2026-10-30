package okay.cardano

import scalus.cardano.ledger.Script
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/**
 * BLUEPRINTS (CIP-57, `plutus.json`) as data: each validator's purpose, its datum's, redeemer's and parameters'
 * schemas, its program. Read here rather than with Scalus's model so that `$ref`s into `definitions` — how Aiken
 * writes every blueprint — are followed.
 */
object Blueprints:

  /** what a value of a schema is made of; `title` the blueprint's name for it, empty if none */
  enum Schema:
    case Integer(title: String)
    case Bytes(title: String)
    case Text(title: String)
    case ListOf(title: String, items: Schema)
    case Tuple(title: String, items: List[Schema])
    case MapOf(title: String, keys: Schema, values: Schema)
    case Constr(title: String, index: Int, fields: List[Schema])
    case AnyOf(title: String, alternatives: List[Schema])
    /** any Data (`{}`, `#/definitions/Data`), a builtin type, or what this reader does not know */
    case Opaque(title: String)

    def title: String
    def named(t: String): Schema = if t.isEmpty then this else this match
      case s: Integer => s.copy(title = t)
      case s: Bytes => s.copy(title = t)
      case s: Text => s.copy(title = t)
      case s: ListOf => s.copy(title = t)
      case s: Tuple => s.copy(title = t)
      case s: MapOf => s.copy(title = t)
      case s: Constr => s.copy(title = t)
      case s: AnyOf => s.copy(title = t)
      case s: Opaque => s.copy(title = t)

  /** a validator of the blueprint: `purpose` from its title's last part (`.spend`, `.mint`, … as Aiken names them) or
   * its redeemer's `purpose`, `None` when the blueprint does not say */
  final case class Validator(title: String, description: String, purpose: Option[String], datum: Option[Schema],
    redeemer: Schema, parameters: List[Schema], compiledCode: String, hash: Option[String]):
    /** the program with its parameters applied, in order */
    def program(params: List[Data]): DeBruijnedProgram =
      params.foldLeft(DeBruijnedProgram.fromCborHex(compiledCode))((p, d) => p $ d)
    /** the script hash of the program with these parameters: its address, its policy id */
    def scriptHash(params: List[Data]): ByteString =
      Script.PlutusV3(ByteString.fromArray(program(params).cborEncoded)).scriptHash

  final case class Read(title: String, plutusVersion: String, validators: List[Validator])

  val purposes: Set[String] = Set("spend", "mint", "withdraw", "publish", "vote", "propose")

  def read(json: String): Either[String, Read] =
    scala.util.Try(ujson.read(json)).toEither.left.map(_ => "not JSON").flatMap { j =>
      scala.util.Try {
        val definitions = j.obj.get("definitions").map(_.obj.toMap).getOrElse(Map.empty)
        def resolve(v: ujson.Value, depth: Int): Schema =
          val o = v.objOpt.getOrElse(collection.mutable.LinkedHashMap.empty[String, ujson.Value])
          val title = o.get("title").flatMap(_.strOpt).getOrElse("")
          if depth > 24 then Schema.Opaque(title)
          else o.get("$ref").flatMap(_.strOpt) match
            case Some(ref) =>
              val name = ref.stripPrefix("#/definitions/").replace("~1", "/").replace("~0", "~")
              definitions.get(name).map(d => resolve(d, depth + 1).named(title)).getOrElse(Schema.Opaque(title))
            case None =>
              (o.get("anyOf"), o.get("dataType").flatMap(_.strOpt)) match
                case (Some(alts), _) =>
                  val all = alts.arr.toList.map(resolve(_, depth + 1))
                  if all.size == 1 then all.head.named(title) else Schema.AnyOf(title, all)
                case (None, Some("integer")) => Schema.Integer(title)
                case (None, Some("bytes")) => Schema.Bytes(title)
                case (None, Some("#string")) => Schema.Text(title)
                case (None, Some("list")) => o.get("items") match
                  case Some(ujson.Arr(items)) => Schema.Tuple(title, items.toList.map(resolve(_, depth + 1)))
                  case Some(item) => Schema.ListOf(title, resolve(item, depth + 1))
                  case None => Schema.ListOf(title, Schema.Opaque(""))
                case (None, Some("map")) =>
                  Schema.MapOf(title, o.get("keys").map(resolve(_, depth + 1)).getOrElse(Schema.Opaque("")),
                    o.get("values").map(resolve(_, depth + 1)).getOrElse(Schema.Opaque("")))
                case (None, Some("constructor")) =>
                  Schema.Constr(title, o.get("index").map(_.num.toInt).getOrElse(0),
                    o.get("fields").map(_.arr.toList.map(resolve(_, depth + 1))).getOrElse(Nil))
                case _ => Schema.Opaque(title)
        def described(v: ujson.Value): Schema =
          val schema = resolve(v.obj.get("schema").getOrElse(ujson.Obj()), 0)
          schema.named(v.obj.get("title").flatMap(_.strOpt).getOrElse(""))
        val preamble = j("preamble")
        val validators = j("validators").arr.toList.map { v =>
          val o = v.obj
          val title = o("title").str
          val suffix = title.split('.').last
          val purpose = Option(suffix).filter(purposes)
            .orElse(o.get("redeemer").flatMap(_.obj.get("purpose")).flatMap(_.strOpt))
          Validator(title, o.get("description").flatMap(_.strOpt).getOrElse(""), purpose,
            o.get("datum").map(described), o.get("redeemer").map(described).getOrElse(Schema.Opaque("redeemer")),
            o.get("parameters").map(_.arr.toList.map(described)).getOrElse(Nil),
            o.get("compiledCode").map(_.str).getOrElse(""), o.get("hash").flatMap(_.strOpt))
        }.filter(v => v.compiledCode.nonEmpty && !v.title.endsWith(".else"))
        Read(preamble.obj.get("title").flatMap(_.strOpt).getOrElse("blueprint"),
          preamble.obj.get("plutusVersion").flatMap(_.strOpt).getOrElse("v3"), validators)
      }.toEither.left.map(e => s"not a CIP-57 blueprint: ${e.getMessage}")
    }

  /** a value of the schema to start from: the first alternative, zeros, empty bytes and lists */
  def skeleton(s: Schema): Data = s match
    case Schema.Integer(_) => BigInt(0).toData
    case Schema.Bytes(_) => ByteString.empty.toData
    case Schema.Text(_) => Data.B(ByteString.empty)
    case Schema.ListOf(_, _) | Schema.MapOf(_, _, _) =>
      if s.isInstanceOf[Schema.MapOf] then Data.Map(scalus.cardano.onchain.plutus.prelude.List.Nil)
      else Data.List(scalus.cardano.onchain.plutus.prelude.List.Nil)
    case Schema.Tuple(_, items) => Data.List(scalus.cardano.onchain.plutus.prelude.List.from(items.map(skeleton)))
    case Schema.Constr(_, i, fields) =>
      Data.Constr(i, scalus.cardano.onchain.plutus.prelude.List.from(fields.map(skeleton)))
    case Schema.AnyOf(_, alts) => alts.headOption.map(skeleton).getOrElse(Data.unit)
    case Schema.Opaque(_) => Data.unit
