package com.github.melvic.sucoban

import fastparse._

import scala.annotation.tailrec
import scala.io.StdIn

object Repl {
  @tailrec
  def loop(): Unit = StdIn.readLine("> ") match {
    case "exit" => println("Bye.")
    case input =>
      val result = parse(input, Parsers.term(_)) match {
        case Parsed.Success(value, _) => Show(value)
        case Parsed.Failure(label, index, extra) => s"An Error occured: $extra"
      }
      println(result)
      Repl.loop()
  }
}
