package com.ayush.FuturesInScala

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object FutureAsSequence {

    def main(args: Array[String]): Unit = {

      println("Starting !!!")


      val x1 = Future {
        Thread.sleep(2000)
        println("first is completed")
        "My name is"
      }

      val x2 = Future {
        Thread.sleep(12000)
        Thread.sleep(2000)
        println("second is completed")
        throw new Exception("Exception occured with x2")
        "Ayush"
      }

      val x3 = Future {
        Thread.sleep(10000)
        println("third is completed")
        " !!"
      }

      val x = Future.sequence(List(x1, x2, x3))

      x.onComplete {
        case Success(res) => println("Success: " + res)
        case Failure(ex)  => println("Ohhh shit some Exception: " + ex.getMessage)
      }

      Await.ready(x, 20.seconds)

      println(s"Future val is: ${x.value}")
      Thread.sleep(20000)
      println(s"Future val is: ${x.value}")
      println("test - end")
    }
  }
