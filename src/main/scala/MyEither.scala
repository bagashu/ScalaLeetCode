/**
  * Created by abagla on 1/16/18.
  */

sealed trait MyEither[+L, +R] {

  def isLeft: Boolean
  def isRight: Boolean

  def fold[X](f1: L => X)(f2: R => X): X ={
    this match {
      case Left(value) => f1(value)
      case Right(value) => f2(value)
    }
  }
}

case class Left[L, Nothing](t: L) extends MyEither[L, Nothing] {
  def isLeft: Boolean = true
  def isRight: Boolean = false
}

case class Right[Nothing, R](t: R) extends MyEither[Nothing, R] {

  override def isLeft: Boolean = false
  override def isRight: Boolean = true
}
