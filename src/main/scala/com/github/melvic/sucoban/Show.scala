package com.github.melvic.sucoban

import com.github.melvic.sucoban.Term.{False, If, IsZero, Pred, Succ, True, Zero}

object Show {
  def apply: Term => String = {
    case True => "true"
    case False => "false"
    case If(condition, ifTrue, ifFalse) =>
      s"if ${Show(ifTrue)} else ${Show(ifFalse)}"
    case Zero => "0"
    case Succ(term) => s"succ ${Show(term)}"
    case Pred(term) => s"pred ${Show(term)}"
    case IsZero(term) => s"isZero ${Show(term)}"
  }
}