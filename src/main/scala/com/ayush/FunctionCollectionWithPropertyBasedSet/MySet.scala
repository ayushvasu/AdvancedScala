package com.ayush.FunctionCollectionWithPropertyBasedSet

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  def apply(elem: A): Boolean = contains(elem)

  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(otherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach(f: A => Unit): Unit

  def -(elem: A): MySet[A]

  //intersection
  def &(anotherSet: MySet[A]): MySet[A]

  //difference
  def --(anotherSet: MySet[A]): MySet[A]

  def unary_! :MySet[A]
}

object MySet {

  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayGround extends App {
  val s = MySet(1, 2, 3, 4)

  println(s(1))

  s.foreach(println)

  s + 5 ++ MySet(-1, -2) + 3 map (x => x * 10) foreach println

  s filter (_ > 2) foreach println

  val negationSet = !s

  println(negationSet(5))
  println(negationSet(2))



}