/**
  * Created by abagla on 3/12/18.
  */

object MyOption {
  def apply[T](ele: T): MyOption[T] = ele match {
    case null => MyNone
    case e => MySome(e)
  }

}

trait MyOption[+T] {
  def isDefined: Boolean
  def isEmpty: Boolean
  def get: T
  def myMap[B](f: T => B): MyOption[B] = if (isDefined) MySome(f(this.get)) else MyNone
  def myFlatMap[B](f: T => MyOption[B]): MyOption[B] = if (isDefined) f(this.get) else MyNone
}

case class MySome[+T](elem: T) extends MyOption[T] {
  def isDefined: Boolean = true
  def isEmpty= false
  def get: T = elem
  }

case object MyNone extends MyOption[Nothing] {
  def isDefined: Boolean = false
  def isEmpty= true
  def get = throw new IllegalArgumentException("hello")
}

object MyObjectTest extends App{
  val x= MySome(3)
  val y= MyNone
  println(x.myMap(r => r+2))
  println(x.myFlatMap(r => MySome(r+2)))



}