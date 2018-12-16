package com.ayush.FunctionCollectionWithPropertyBasedSet

class PropertyBasedSet[A](property: A=>Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || x == elem)

  override def ++(otherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => property(x) || otherSet(x))

  override def map[B](f: A => B): MySet[B] = politeError

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politeError

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) || predicate(x))

  override def foreach(f: A => Unit): Unit = politeError

  override def -(elem: A): MySet[A] = filter(x => x != elem)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politeError = throw new IllegalArgumentException("Function not allowed for property based set")
}
