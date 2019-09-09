package com.github.melvic.sucoban

import com.github.melvic.sucoban.Term.{False, If, IsZero, Pred, Succ, True, Zero}

object Show {
  def apply(term: Term): String = term match {
    case True => "true"
    case False => "false"
    case If(condition, ifTrue, ifFalse) =>
      s"if ${Show(condition)} ${Show(ifTrue)} else ${Show(ifFalse)}"
    case Zero => "0"
    case succ: Succ => toNumber(succ).toString
    case pred: Pred => toNumber(pred).toString
    case IsZero(term) => s"isZero ${Show(term)}"
  }

  def toNumber: Term => Int = {
    case Succ(Zero) => 1
    case Succ(term) => 1 + toNumber(term)
    case Pred(Zero) => 0
    case Pred(term) =>
      val x = toNumber(term)
      if (x > 0) x - 1 else 0
  }
}