package com.ayush

object AdvancedInheritance extends App {

  trait Animal{def name: String}
  trait Lion extends Animal{override def name: String = "Lion"}
  trait Tiger extends Animal{override def name: String = "Tiger"}
  class Mutant extends Lion with Tiger

  println(new Mutant().name) /*
  output : Tiger

  Last override gets picked

  Mutant
  extends Animal with {override name = lion}
  with Animal with {override name = tiger}
  ||
  \/
  Mutant extends Animal with {override name = tiger}
  */

  //----------------------------------------------------

  /*Type Linearization*/

  trait Cold {
    def print = println("cold")
  }

  trait Green extends Cold {
    override def print = {
      println("Green")
      super.print
    }
  }

  trait Blue extends Cold {
    override def print = {
      println("Blue")
      super.print
    }
  }

  class Red {
    def print = println("Red")
  }

  class White extends Red with Green with Blue {
    override def print: Unit = {
      println("White")
      super.print
    }
  }

  new White().print/*
  output :  White
            Blue
            Green
            cold

  Cold = AnyRef with <Cold>
  Green = AnyRef with <Cold> with <Green>
  Blue = AnyRef with <Cold> with <Blue>
  Red = AnyRef with <Red>

  So,
  White =

  AnyRef with <red> with <cold> with <green> with <blue> with <white> // -> TypeLinearization

  In this case Super call the exact right of the hierarchy

  So

  White -super-> Blue -super-> Green -super-> Cold

  * */


}
