package com.ayush

object PartialFunctions extends App {
  val partialFunction : PartialFunction[Int,Int] = {
    case 1 => 2
    case 2 => 3
    case 4 => 5
  }

  println(partialFunction.isDefinedAt(7))

  val lifted = partialFunction.lift

  println(partialFunction(4))
  println(partialFunction(2))

  val pfChain = partialFunction.orElse[Int, Int]{
    case 6 => 7
  }

  println(pfChain(1))
  println(pfChain(6))

  val aManualFussyFunction = new PartialFunction[Int,Int] {
    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2 || x == 3

    override def apply(v1: Int): Int = v1 match {
      case 1 => 2
      case 2 => 3
      case 3 => 4
    }
  }

  val chatBot:PartialFunction[String, String] = {
    case "Hello" => "Hello! I am chatBot"
    case "Good morning" => "Morning"
    case _ => "can't understand"
  }

  scala.io.Source.stdin.getLines().map(chatBot).foreach(println)

}
