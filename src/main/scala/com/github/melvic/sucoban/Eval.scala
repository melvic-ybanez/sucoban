package com.github.melvic.sucoban

import com.github.melvic.sucoban.Term._

import scala.annotation.tailrec
import scala.util.Try

object Eval {
  @tailrec
  def apply(term: Term): Term = Try(partialEval(term)).toEither match {
    case Left(_) => term
    case Right(partiallyEvaluated) => Eval(partiallyEvaluated)
  }

  def partialEval: Term => Term = {
    case If(True, ifTrue, _) => ifTrue
    case If(False, _, ifFalse) => ifFalse
    case If(condition, ifTrue, ifFalse) => If(partialEval(condition), ifTrue, ifFalse)

    case Succ(term) => Succ(partialEval(term))
    case Pred(Zero) => Zero
    case Pred(Succ(term)) if zeroOrSucc(term) => term
    case Pred(term) => Pred(partialEval(term))

    case IsZero(Zero) => True
    case IsZero(Succ(term)) if zeroOrSucc(term) => False
    case IsZero(term) => IsZero(partialEval(term))
  }

  def zeroOrSucc: Term => Boolean = {
    case Zero => true
    case Succ(term) => zeroOrSucc(term)
    case _ => false
  }
}
