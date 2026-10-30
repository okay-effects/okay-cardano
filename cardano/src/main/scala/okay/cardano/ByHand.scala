package okay.cardano

import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.onchain.plutus.v1.{Address, IntervalBound, IntervalBoundType, PubKeyHash}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}

/**
 * A SCRIPT CONTEXT BY HAND, for a script with no transaction yet (a blueprint's validator): the fields a person says —
 * who signs, when the transaction is valid, what it mints, whom it pays — the rest empty. For a spend, the output being
 * spent is an input of the transaction at the script's address, holding `value` lovelace and the datum inline.
 */
object ByHand:
  enum Purpose:
    case Spend(ownRef: TxOutRef, datum: Option[Data], lovelace: BigInt)
    case Mint

  /** a payment: to a key's address, lovelace, a datum inline if any */
  final case class Payment(to: ByteString, lovelace: BigInt, datum: Option[Data])

  /** minted (or burned, negative): under `policy`, `None` being the script's own */
  final case class Minted(policy: Option[ByteString], name: ByteString, quantity: BigInt)

  /** `spends`: other outputs the transaction spends (a seed for a one-shot policy), at an address of no one's key */
  final case class Fields(signatories: List[ByteString] = Nil, validFrom: Option[BigInt] = None,
    validTo: Option[BigInt] = None, mint: List[Minted] = Nil, payments: List[Payment] = Nil,
    spends: List[TxOutRef] = Nil)

  def context(scriptHash: ByteString, purpose: Purpose, redeemer: Data, f: Fields): ScriptContext =
    val lower = f.validFrom.fold(IntervalBound(IntervalBoundType.NegInf, true))(t =>
      IntervalBound(IntervalBoundType.Finite(t), true))
    val upper = f.validTo.fold(IntervalBound(IntervalBoundType.PosInf, true))(t =>
      IntervalBound(IntervalBoundType.Finite(t), false))
    val minted = f.mint.foldLeft(Value.zero)((v, m) => v + Value(m.policy.getOrElse(scriptHash), m.name, m.quantity))
    val outputs = f.payments.map(p => TxOut(Address.fromPubKeyHash(PubKeyHash(p.to)), Value.lovelace(p.lovelace),
      p.datum.fold(OutputDatum.NoOutputDatum)(OutputDatum.OutputDatum(_))))
    val (inputs, info) = purpose match
      case Purpose.Spend(ref, datum, lovelace) =>
        val own = TxOut(Address.fromScriptHash(scriptHash), Value.lovelace(lovelace),
          datum.fold(OutputDatum.NoOutputDatum)(OutputDatum.OutputDatum(_)))
        (PList(TxInInfo(ref, own)), ScriptInfo.SpendingScript(ref, datum.fold(POption.None)(POption.Some(_))))
      case Purpose.Mint => (PList.Nil, ScriptInfo.MintingScript(scriptHash))
    val others = f.spends.map(r => TxInInfo(r, TxOut(Address.fromPubKeyHash(PubKeyHash(ByteString.fromArray(
      Array.fill(28)(0.toByte)))), Value.lovelace(2_000_000), OutputDatum.NoOutputDatum)))
    val tx = TxInfo.placeholder.copy(
      inputs = PList.from(inputs.toScalaList ++ others),
      outputs = PList.from(outputs),
      mint = minted,
      signatories = PList.from(f.signatories.map(PubKeyHash(_))),
      validRange = Interval(lower, upper)
    )
    ScriptContext(tx, redeemer, info)
