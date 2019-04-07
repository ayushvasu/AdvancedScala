package com.ayush.ImplicitsThings

object JsonWithImplicitTypeClass extends App {

  case class User(name:String, email:String)
  case class Post(msg:String)
  case class Feed(user:User, posts:List[Post])

  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = "\"" + value + "\""
  }
  final case class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }
  final case class JSONArray(value: List[JSONValue]) extends JSONValue {
    override def stringify: String = value.map(_.stringify).mkString("[",",","}")
  }
  final case class JSONObject[T](value: Map[String, JSONValue]) extends JSONValue {
    override def stringify: String = value.map{
      case (key, value) => "\"" + key + "\" : " + value.stringify
    }.mkString("{",",","}")
  }

  //------converters

  trait JSONConverter[T] {
    def convert(value:T): JSONValue
  }

  implicit class JSONOps[T](value: T) {
    def toJSON(implicit converter: JSONConverter[T]): JSONValue = converter.convert(value)
  }

  implicit object StringConverter extends JSONConverter[String] {
    override def convert(value: String): JSONValue = JSONString(value)
  }

  implicit object NumberConverter extends JSONConverter[Int] {
    override def convert(value: Int): JSONValue = JSONNumber(value)
  }

  implicit object UserConverter extends JSONConverter[User] {
    override def convert(value: User): JSONValue = JSONObject {
      Map(
        "name" -> value.name.toJSON,//JSONString(value.name),
        "email" -> value.email.toJSON//JSONString(value.email)
      )
    }
  }

  implicit object PostConverter extends JSONConverter[Post] {
    override def convert(value: Post): JSONValue = value.msg.toJSON//JSONString(value.msg)
  }


  implicit object FeedConverter extends JSONConverter[Feed] {
    override def convert(value: Feed): JSONValue = JSONObject {
      Map(
        "USER" -> value.user.toJSON,//UserConverter.convert(value.user),
        "POSTS" -> JSONArray(value.posts.map(_.toJSON))
       )
    }
  }

  val jhon = User("Jhon", "J@gmail.com")
  val feed = Feed(jhon, List(Post("ayush1"),Post("ayush2")))

  println(feed.toJSON.stringify)


}
