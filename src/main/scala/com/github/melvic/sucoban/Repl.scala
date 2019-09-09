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
        case Parsed.Failure(_, _, extra) => s"An Error occurred: $extra"
      }
      println(result)
      Repl.loop()
  }
}
