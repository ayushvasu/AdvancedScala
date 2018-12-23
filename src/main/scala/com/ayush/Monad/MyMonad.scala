package com.ayush.Monad

object MyMonad extends App {

  trait Attempt[+A]{
    def flatMap[B](f: A => Attempt[B]):Attempt[B]
  }

  object Attempt{
    def apply[A](a: =>A): Attempt[A] = try {
      Success(a)
    } catch {
      case e :Throwable => Fail(e)
    }
  }

  case class Success[A](value: A) extends Attempt[A]{
    def flatMap[B](f: A => Attempt[B]):Attempt[B] = try {
      f(value)
    }catch {
      case e :Throwable => Fail(e)
    }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  /*
  * Left Identity - unit(x).flatMap(f) = f(x)
  *                 Attempt(x).flatMap(f) = f(x)
  *                 Success(x).fltMap(f) = f(x) ==> Proved
  *
  * Right Identity - monad.flatMap(unit) = monad
  *                  Success(x).flatMap(x => Attempt(x)) = Success(x) ==> Proved
  *
  * Associativity - attempt.flatMap(f).flatMap(g) = attempt.flatMap(x => f(x).flatMap(g))
  *                 Fail(e).flatMap(.....) = Fail(e) ==> Proved
  *                 Success(x).flatMap(f).flatMap(g) = g(f(x)) ==> Proved
  * */

  val attemptMonad = Attempt{
    1 / 0
  }
  println(attemptMonad)

  attemptMonad match {
    case Success(v) => println(v)
    case Fail(e) => println(e)
  }
}
