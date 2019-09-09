import Term.{False, If, IsZero, Pred, Succ, True, Zero}

sealed trait Console

object Console {
  implicit val consolePrinter: Printer[Console, String] = new Printer[Console, String] {
    override def printTerm(instance: Console, term: Term): String = term match {
      case True => "true"
      case False => "false"
      case If(condition, ifTrue, ifFalse) =>
        s"if ${printTerm(instance, ifTrue)} else ${printTerm(instance, ifFalse)}"
      case Zero => "0"
      case Succ(term) => s"succ ${printTerm(instance, term)}"
      case Pred(term) => s"pred ${printTerm(instance, term)}"
      case IsZero(term) => s"isZero ${printTerm(instance, term)}"
    }
  }
}
