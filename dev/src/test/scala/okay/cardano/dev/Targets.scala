package okay.cardano.dev

import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

/** the attacks' targets: each safe contract beside a copy with ONE check removed, whose attack must then succeed */
case class Sale(seller: PubKeyHash, price: BigInt) derives FromData, ToData

enum SaleAction derives FromData, ToData:
  case Buy
  case Cancel

/** pays the seller, any output: double satisfaction */
@Compile
object LooseSale extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val d = datum.getOrFail("no datum").to[Sale]
    val seller = Credential.PubKeyCredential(d.seller)
    require(tx.outputs.exists(o => o.address.credential === seller && o.value.getLovelace >= d.price), "unpaid")
}

/** pays the seller something: the amount not checked */
@Compile
object CarelessSale extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val d = datum.getOrFail("no datum").to[Sale]
    val seller = Credential.PubKeyCredential(d.seller)
    require(tx.outputs.exists(o => o.address.credential === seller), "unpaid")
}

/** pays the seller in an output tagged with this UTxO */
@Compile
object TaggedSale extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val d = datum.getOrFail("no datum").to[Sale]
    val seller = Credential.PubKeyCredential(d.seller)
    val tag = ownRef.toData
    require(tx.outputs.exists(o => o.address.credential === seller && o.value.getLovelace >= d.price
      && (o.datum match
        case OutputDatum.OutputDatum(t) => t == tag
        case _ => false)), "unpaid")
}

/** a counter whose state carries the token of its own policy; `Strict` checks everything, the others one check less */
@Compile
object Counter {
  def name: ByteString = ByteString.fromString("STATE")
  def only(v: Value, own: ByteString): Boolean =
    v.quantityOf(own, name) == BigInt(1) && v.withoutLovelace.flatten.length == BigInt(1)
}

@Compile
object StrictCounter extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val own = tx.findInputOrFail(ownRef, "own").resolved
    val h = own.address.credential match
      case Credential.ScriptCredential(h) => h
      case _ => fail("not a script")
    require(own.value.quantityOf(h, Counter.name) == BigInt(1), "not the state")
    require(tx.findInputsByCredential(own.address.credential).length == BigInt(1), "one input")
    val outs = tx.findOutputsByCredential(own.address.credential)
    require(outs.length == BigInt(1), "one output")
    require(outs.head.address === own.address, "same address")
    require(Counter.only(outs.head.value, h), "only the state")
}

/** no token check: a forged state passes */
@Compile
object UnauthCounter extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val own = tx.findInputOrFail(ownRef, "own").resolved
    require(tx.findInputsByCredential(own.address.credential).length == BigInt(1), "one input")
    val outs = tx.findOutputsByCredential(own.address.credential)
    require(outs.length == BigInt(1), "one output")
    require(outs.head.address === own.address, "same address")
}

/** the payment credential compared, not the whole address: the stake part can be changed */
@Compile
object StakeBlindCounter extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val own = tx.findInputOrFail(ownRef, "own").resolved
    val h = own.address.credential match
      case Credential.ScriptCredential(h) => h
      case _ => fail("not a script")
    require(own.value.quantityOf(h, Counter.name) == BigInt(1), "not the state")
    require(tx.findInputsByCredential(own.address.credential).length == BigInt(1), "one input")
    val outs = tx.findOutputsByCredential(own.address.credential)
    require(outs.length == BigInt(1), "one output")
    require(Counter.only(outs.head.value, h), "only the state")
}

/** the state token kept, anything else allowed beside it: dust */
@Compile
object DustyCounter extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val own = tx.findInputOrFail(ownRef, "own").resolved
    val h = own.address.credential match
      case Credential.ScriptCredential(h) => h
      case _ => fail("not a script")
    require(own.value.quantityOf(h, Counter.name) == BigInt(1), "not the state")
    require(tx.findInputsByCredential(own.address.credential).length == BigInt(1), "one input")
    val outs = tx.findOutputsByCredential(own.address.credential)
    require(outs.length == BigInt(1), "one output")
    require(outs.head.address === own.address, "same address")
    require(outs.head.value.quantityOf(h, Counter.name) == BigInt(1), "the state kept")
}

/** the state token under a policy of its own (a constant here): checked */
@Compile
object ForeignStateCounter extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val own = tx.findInputOrFail(ownRef, "own").resolved
    val policy = ByteString.fromHex("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    require(own.value.quantityOf(policy, Counter.name) == BigInt(1), "not the state")
    require(tx.findInputsByCredential(own.address.credential).length == BigInt(1), "one input")
    val outs = tx.findOutputsByCredential(own.address.credential)
    require(outs.length == BigInt(1), "one output")
    require(outs.head.address === own.address, "same address")
    require(outs.head.value.quantityOf(policy, Counter.name) == BigInt(1), "the state kept")
}

/** dust accepted, and the value compared token by token against the spent one: cost quadratic in the dust */
@Compile
object QuadraticCounter extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val own = tx.findInputOrFail(ownRef, "own").resolved
    val h = own.address.credential match
      case Credential.ScriptCredential(h) => h
      case _ => fail("not a script")
    require(own.value.quantityOf(h, Counter.name) == BigInt(1), "not the state")
    require(tx.findInputsByCredential(own.address.credential).length == BigInt(1), "one input")
    val outs = tx.findOutputsByCredential(own.address.credential)
    require(outs.length == BigInt(1), "one output")
    require(outs.head.address === own.address, "same address")
    val before = own.value.flatten
    require(outs.head.value.flatten.forall(t => before.exists(_ === t)), "the value kept")
}

/** a queue anyone joins: the joiner signs, is not in it yet, and is put in front */
case class Queue(owner: PubKeyHash, members: List[PubKeyHash]) derives FromData, ToData

@Compile
object LinearQueue extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val q = datum.getOrFail("no datum").to[Queue]
    val own = tx.findInputOrFail(ownRef, "own").resolved
    val joiner = tx.signatories.head
    require(!q.members.exists(_ === joiner), "already in")
    val outs = tx.findOutputsByCredential(own.address.credential)
    require(outs.length == BigInt(1), "one output")
    val next = outs.head.datum match
      case scalus.cardano.onchain.plutus.v2.OutputDatum.OutputDatum(d) => d.to[Queue]
      case _ => fail("inline datum")
    require(next.owner === q.owner && next.members === List.Cons(joiner, q.members), "joined in front")
}

/** the same, and every member checked unique against all the others: cost quadratic in the queue */
@Compile
object QuadraticQueue extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val q = datum.getOrFail("no datum").to[Queue]
    val own = tx.findInputOrFail(ownRef, "own").resolved
    val joiner = tx.signatories.head
    val outs = tx.findOutputsByCredential(own.address.credential)
    require(outs.length == BigInt(1), "one output")
    val next = outs.head.datum match
      case scalus.cardano.onchain.plutus.v2.OutputDatum.OutputDatum(d) => d.to[Queue]
      case _ => fail("inline datum")
    require(next.owner === q.owner && next.members === List.Cons(joiner, q.members), "joined in front")
    require(next.members.forall(m => next.members.filter(_ === m).length == BigInt(1)), "members unique")
}

/** mints exactly one STATE, or burns it */
@Compile
object StrictPolicy extends Validator {
  inline override def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit =
    val minted = tx.mint.tokens(policyId)
    if redeemer.to[BigInt] == BigInt(0) then require(tx.mint.hasOnly(policyId, Counter.name, BigInt(1)), "one")
    else require(minted.forall((_, q) => q < BigInt(0)), "only burns")
}

/** counts STATE only: other names and burns that mint pass */
@Compile
object LoosePolicy extends Validator {
  inline override def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit =
    if redeemer.to[BigInt] == BigInt(0) then require(tx.mint.quantityOf(policyId, Counter.name) == BigInt(1), "one")
    else require(tx.mint.quantityOf(policyId, Counter.name) < BigInt(0), "burns")
}

object Targets {
  private given Options = Options.release
  lazy val looseSale = PlutusV3.compile(LooseSale.validate)
  lazy val taggedSale = PlutusV3.compile(TaggedSale.validate)
  lazy val carelessSale = PlutusV3.compile(CarelessSale.validate)
  lazy val strictCounter = PlutusV3.compile(StrictCounter.validate)
  lazy val unauthCounter = PlutusV3.compile(UnauthCounter.validate)
  lazy val stakeBlindCounter = PlutusV3.compile(StakeBlindCounter.validate)
  lazy val dustyCounter = PlutusV3.compile(DustyCounter.validate)
  lazy val foreignStateCounter = PlutusV3.compile(ForeignStateCounter.validate)
  lazy val quadraticCounter = PlutusV3.compile(QuadraticCounter.validate)
  lazy val linearQueue = PlutusV3.compile(LinearQueue.validate)
  lazy val quadraticQueue = PlutusV3.compile(QuadraticQueue.validate)
  lazy val strictPolicy = PlutusV3.compile(StrictPolicy.validate)
  lazy val loosePolicy = PlutusV3.compile(LoosePolicy.validate)
}
