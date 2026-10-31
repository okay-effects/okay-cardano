package okay.cardano.dev

import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data
import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

/** a validator of the tests' own: spends only when signed by the key in its datum */
@Compile
object Gate extends Validator {
  inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
    val owner = datum.getOrFail("no datum").to[PubKeyHash]
    require(tx.isSignedBy(owner), "the owner must sign")
}

object GateContract extends Contract {
  private given Options = Options.release
  lazy val compiled = PlutusV3.compile(Gate.validate)
  lazy val blueprint = Blueprint.plutusV3[PubKeyHash, Unit](title = "Gate", description = "signed by its owner",
    version = "1.0.0", license = None, compiled = compiled)
}
