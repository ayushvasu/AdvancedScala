package com.ayush.FuturesInScala

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

object PromiseBasic extends App {

  val promise = Promise[Int]()
  val future = promise.future

  future.onComplete{
    case Success(r) => println("consuming "+r)
  }

  val producer = new Thread{() =>
  println("producer")
    Thread.sleep(1000)
    promise success 42
    println("done")
  }

  producer.start()

  Thread.sleep(3000)

}
