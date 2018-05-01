package scala_school

import util.Random._
/**
  * Created by abagla on 4/18/18.
  */
class practice {

}

object test extends App{
//  val obviousAlready = if (nextBoolean()) new A else new B
  val c = if (nextBoolean()) 42
  def twist(s:String) = s.reverse
  def shout(s:String) = s.toUpperCase + "!"

  def twistAndShout(s: String) = twist _ andThen shout _
  def shoutedTwist  = twist _ compose shout _

  trait B1 {
  }

  trait A {
    trait B2 extends B1
  }

  val b1 = new B1 {}

  val v1 = new A with B1


  println(twistAndShout("Shaking"))
  println(shoutedTwist("Shaking"))
}
