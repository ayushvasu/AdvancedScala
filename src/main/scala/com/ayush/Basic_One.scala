package com.ayush

import scala.annotation.tailrec

object Basic_One extends App {

  @tailrec
  def factorial(n:BigInt, acc:BigInt):BigInt ={
    if(n <= 0) acc else factorial(n-1, n * acc)
  }

  println(factorial(math.BigInt.int2bigInt(100), math.BigInt.int2bigInt(1)))

  trait Car {
    def brand(name:String) : String
  }
  val audi = new Car {
    override def brand(name: String): String = "Audi "+name
  }

  val bmw : Car = (name:String) => "Bmw " + name

  val aSweetThread = new Thread(() => println("Ayush Vasu"))

  val PrePendedList = 2 :: List(1,2)
  // : is right associative => associativity determine by the last char of the exp
  println(PrePendedList)
  println(List(1,2).::(3))


  class MyStream[T] {
    def -->:(value:T): MyStream[T] = this
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  def `and Then Ayush Say`(someThing : String) = someThing

  println(`and Then Ayush Say`("YOYO"))

  class Composite[A,B]
  //val toWards: Int Composite Int = ???

  //Getter and setter
  class Mutable {
    private var internalNumber = 0
    def member : Int = internalNumber
    def member_=(value: Int) = {internalNumber = value}
  }

  val ob = new Mutable
  ob.member = 34

  //Advanced Pattern Matching with unapply
  class Person(val name:String, val age :Int)

  object Person{
    def unapply(person: Person):Option[(String,Int)] = Some((person.name,person.age))
  }

  new Person("Ayush",24) match {
    case Person(a,b) => println("here it is "+a+"  "+b)
  }

  case class Or[A, B](a:A, b:B)
  val either = Or(2, "Ayush")
  val h = either match {
    case number Or string => println("Here It is")
  }

  //Decomposing Sequence

  val number = List(1,2,3,4,5,6,7,8)

  number match {
    case List(1, _*) => println("List start with number")
  }

  abstract class MyList[+A]{
    def head:A = ???
    def tail:MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A,
                      override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if(list == Empty) Some(Seq.empty) else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList = Cons(1, Cons(2, Cons(3, Empty)))

  myList match {
    case MyList(1, 2, _*) => println("MyList start with 1 2 ----")
    case _ => println("some thing is wrong")
  }
}
