package com.github.melvic.sucoban

sealed trait Term

object Term {
  case object True extends Term
  case object False extends Term
  final case class If(condition: Term, ifTrue: Term, ifFalse: Term) extends Term
  case object Zero extends Term
  final case class Succ(term: Term) extends Term
  final case class Pred(term: Term) extends Term
  final case class IsZero(term: Term) extends Term
}
