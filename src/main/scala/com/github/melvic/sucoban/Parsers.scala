package com.github.melvic.sucoban

import com.github.melvic.sucoban.Term._
import fastparse._
import SingleLineWhitespace._

object Parsers {
  def `true`[_: P] = P("true").map(_ => True)
  def `false`[_: P] = P("false").map(_ => False)
  def zero[_: P] = P("0").map(_ => Zero)

  def ifCondition[_: P]: P[If] = P("if" ~ term ~ term ~ "else" ~ term).map {
    case (condition, ifTrue, ifFalse) => If(condition, ifTrue, ifFalse)
  }

  def succ[_: P] = P("succ" ~ term).map(Succ)
  def pred[_: P] = P("pred" ~ term).map(Pred)

  def isZero[_: P] = P("isZero" ~ term).map(IsZero)

  def term[_: P]: P[Term] = P(ifCondition | `true` | `false` | zero | succ | pred | isZero)
}
