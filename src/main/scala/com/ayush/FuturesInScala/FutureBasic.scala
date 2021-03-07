package com.ayush.FuturesInScala

import scala.util.{Failure, Random, Success}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global


object FutureBasic extends App {

  case class Profile(id: Int, name: String){
    def poke (anotherProfile: Profile): Unit =
      println(s"${this.name} is poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    val nameMap = Map (
      1 -> "ayush",
      2 -> "vasu"
      )

    val friend = Map(1->2)

    def fetchProfile(id: Int) = Future{
      Thread.sleep(Random.nextInt(400))
      Profile(id, nameMap(id))
    }

    def fetchBestFriend(profile:Profile): Future[Profile] = Future {
      Thread.sleep(Random.nextInt(400))
      val bfId = friend(profile.id)
      Profile(bfId, nameMap(bfId))
    }
  }

  val mark = SocialNetwork.fetchProfile(1)

  mark.onComplete{
    case Success(markProfile) =>
      val bill = SocialNetwork.fetchBestFriend(markProfile)
      bill.onComplete{
        case Success(billProfile) => markProfile.poke(billProfile)
        case Failure(e) => e.printStackTrace()
      }
    case Failure(e) => e.printStackTrace()
  }

  Thread.sleep(1000)

  /*
  * But better way to handle futures is using map, flatMap and filters
  * Which can further used in for comprehension
  * */

  for{
    mark <- SocialNetwork.fetchProfile(1)
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark.poke(bill)

  Thread.sleep(1000)

  /*
  * Recovery from failure in future
  * Use `recover`
  * `recoverWith`
  * `fallBack`
  * */

  val a = SocialNetwork.fetchProfile(3) recover {
    case e: Throwable =>
      e.printStackTrace()
      Profile(-1, "nothing")
  }

  val b = SocialNetwork.fetchProfile(3).recoverWith{
    case e:Throwable =>
      e.printStackTrace()
      SocialNetwork.fetchProfile(4)
  }

  /*
  * If second argument s also failed than c will store exception of first future
  * */
  val c = SocialNetwork.fetchProfile(3).fallbackTo(SocialNetwork.fetchProfile(1))


  Thread.sleep(1000)

  val aFuture = Future {
    Thread.sleep(1000)
    42
  }

  import scala.concurrent.duration._
  val value = Await.result(aFuture, 5.seconds)
  println(value)

  import scala.concurrent.blocking

  def synchronizedMethod(str: String) = println(str);123

  val f = Future{
    println("Hello")
    val i = blocking{  //Adjust the execution context behavior
      synchronizedMethod("Hello")
    }
    println(i)
    i
  }




}
