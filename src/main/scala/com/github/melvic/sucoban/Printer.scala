package com.github.melvic.sucoban

trait Printer[A, O] {
  def printTerm(instance: A, term: Term): O
}

object Printer {
  def apply[A, O](implicit printer: Printer[A, O]) = printer
}