import fastparse._
import ScalaWhitespace._
import Term.{False, If, IsZero, Pred, Succ, True, Zero}

object Parsers {
  def `true`[_: P] = P("true").map(_ => True)
  def `false`[_: P] = P("false").map(_ => False)
  def zero[_: P] = P("0").map(_ => Zero)

  def ifCondition[_: P] = P("if" ~ term ~ term ~ "else" ~ term).map { case (condition, ifTrue, ifFalse) =>
    If(condition, ifTrue, ifFalse)
  }

  def succ[_: P] = P("succ" ~ term).map(Succ)
  def pred[_: P] = P("pred" ~ term).map(Pred)

  def isZero[_: P] = P("isZero" ~ term).map(IsZero)

  def term[_: P]: P[Term] = P(`true` | `false` | zero | ifCondition | succ | pred | isZero)
}
