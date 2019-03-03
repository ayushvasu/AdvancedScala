package com.ayush

object Sorting extends App {

  val a = List(123, 123, 123,2,412,14,3434,567,112,1,4,2,32,6,3432)

  def quickSort(xs: List[Int]): List[Int] = {
    if (xs.length <= 1)
      xs
    else{
      val pivot = xs.head
      quickSort(xs.filter(pivot > _)) ::: xs.filter(pivot == _) ::: quickSort(xs.filter(pivot<_))
    }
  }

  println(quickSort(a))


  def quickSort2[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    if (xs.length < 2) xs
    else {
      val head = xs.head
      val (f, s) = xs.tail.partition { x => lt(x, head) }
      quickSort2(f)(lt) ::: (head :: quickSort2(s)(lt))
    }
  }

  println(quickSort2(a)((a, b) => a < b))

  def merge(xs:List[Int], ys:List[Int]):List[Int]= (xs,ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xss, y :: yss) => if(x<y) List(x) ::: merge(xss, ys) else List(y) ::: merge(xs, yss)
  }

  def mergeSort(x : List[Int]):List[Int] = {
    if(x.size <= 1){
      x
    }else {
      val (xs, ys) = x.splitAt(x.size/2)
      merge(mergeSort(xs), mergeSort(ys))
    }
  }
  println(mergeSort(a))

  def insertSort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case x :: ys => insert(x, insertSort(ys))
  }
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
}
