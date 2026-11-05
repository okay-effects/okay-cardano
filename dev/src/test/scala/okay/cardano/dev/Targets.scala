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

/** pays the seller, any output: double satisfaction */
@Compile
object LooseSale extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val d = datum.getOrFail("no datum").to[Sale]
    val seller = Credential.PubKeyCredential(d.seller)
    require(tx.outputs.exists(o => o.address.credential === seller && o.value.getLovelace >= d.price), "unpaid")
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
  lazy val strictCounter = PlutusV3.compile(StrictCounter.validate)
  lazy val unauthCounter = PlutusV3.compile(UnauthCounter.validate)
  lazy val stakeBlindCounter = PlutusV3.compile(StakeBlindCounter.validate)
  lazy val dustyCounter = PlutusV3.compile(DustyCounter.validate)
  lazy val strictPolicy = PlutusV3.compile(StrictPolicy.validate)
  lazy val loosePolicy = PlutusV3.compile(LoosePolicy.validate)
}
