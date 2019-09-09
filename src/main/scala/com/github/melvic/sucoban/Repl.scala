package com.github.melvic.sucoban

import fastparse._

import scala.annotation.tailrec
import scala.io.StdIn

object Repl {
  @tailrec
  def apply(): Unit = StdIn.readLine("> ") match {
    case "exit" => println("Bye.")
    case input =>
      val result = parse(input, Parsers.term(_)) match {
        case Parsed.Success(value, _) => Show(Eval(value))
        case failure : Parsed.Failure => s"An Error occurred: ${failure.msg}"
      }
      println(result)
      Repl()
  }
}
