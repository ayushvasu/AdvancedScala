package com.ayush.FunctionCollectionWithPropertyBasedSet

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = head == elem || tail.contains(elem)

  override def +(elem: A): MySet[A] = if (this contains elem) this else new NonEmptySet(elem, this)

  override def ++(otherSet: MySet[A]): MySet[A] = tail ++ otherSet + head

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val predicatedList = tail filter predicate
    if (predicate(head))
      predicatedList + head
    else
      predicatedList
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def -(elem: A): MySet[A] = if(head == elem) tail else tail - elem + head

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  //Because MySet is extending A => Boolean this means here anotherSet work as predicate
  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}