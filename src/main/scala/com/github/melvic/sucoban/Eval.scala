package com.github.melvic.sucoban

import com.github.melvic.sucoban.Term._

object Eval {
  def apply(term: Term): Term = term match {
    case If(True, ifTrue, _) => Eval(ifTrue)
    case If(False, _, ifFalse) => Eval(ifFalse)
    case If(condition, ifTrue, ifFalse) => Eval(If(Eval(condition), ifTrue, ifFalse))

    case Succ(term) => Eval(Succ(Eval(term)))
    case Pred(Zero) => Zero
    case Pred(Succ(term)) if zeroOrSucc(term) => Eval(term)
    case Pred(term) => Eval(Pred(Eval(term)))

    case IsZero(Zero) => True
    case IsZero(Succ(term)) if zeroOrSucc(term) => False
    case IsZero(term) => Eval(IsZero(Eval(term)))
  }

  def zeroOrSucc: Term => Boolean = {
    case Zero => true
    case Succ(term) => zeroOrSucc(term)
    case _ => false
  }
}
