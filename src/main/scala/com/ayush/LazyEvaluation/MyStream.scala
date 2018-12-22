package com.ayush.LazyEvaluation

import java.util.function.Predicate

import scala.annotation.tailrec

abstract class MyStream[+A] {

  def isEmpty: Boolean

  def head: A

  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] //Prepend
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): MyStream[B]

  def flatMap[B](f: A => MyStream[B]): MyStream[B]

  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A]

  def takeAsList(n: Int): List[A]

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse else tail.toList(head :: acc)
}

object EmptyStream extends MyStream[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new RuntimeException("EmptyStream head")

  override def tail: MyStream[Nothing] = throw new RuntimeException("EmptyStream tail")

  override def #::[B >: Nothing](element: B): MyStream[B] = new Cons[B](element, this)

  override def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  override def foreach(f: Nothing => Unit): Unit = ()

  override def map[B](f: Nothing => B): MyStream[B] = this

  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  override def take(n: Int): MyStream[Nothing] = this

  override def takeAsList(n: Int): List[Nothing] = Nil

  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this
}


class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  override def isEmpty: Boolean = false

  override val head: A = hd

  override lazy val tail: MyStream[A] = tl

  override def #::[B >: A](element: B): MyStream[B] = new Cons[B](element, this)

  override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons[B](head, tail ++ anotherStream)

  override def foreach(f: A => Unit): Unit = {
    var these: MyStream[A] = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  override def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f))

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  override def take(n: Int): MyStream[A] = if (n <= 0) EmptyStream
  else if (n == 1) new Cons(head, EmptyStream)
  else new Cons(head, tail.take(n - 1))

  override def takeAsList(n: Int): List[A] = take(n).toList()

  override def filter(predicate: A => Boolean): MyStream[A] = if (predicate(head)) new Cons(head, tail.filter(predicate)) else tail.filter(predicate)
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = new Cons(start, MyStream.from(generator(start))(generator))
}

object Main extends App {
  val a = MyStream.from(3)(_ + 1)
  a.take(50000).foreach(println)

  val b = a.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))) //.take(10).foreach(println())
  b.take(10).foreach(println)


  //fibonacci
  def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] =
    new Cons(first, fibonacci(second, first + second))

  println(fibonacci(0, 1).take(100).toList().mkString("\n"))

  def erathosthenes(number: MyStream[Int]): MyStream[Int] = {
    if (number.isEmpty)
      number
    else
      new Cons(number.head,
        erathosthenes(number.tail.filter(n => n % number.head != 0))
      )
  }

  erathosthenes(MyStream.from(2)(_+1)).take(10).foreach(println)

  println("done!!!!!!!")
}
