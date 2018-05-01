import scala.util.control.NonFatal

/**
  * Created by abagla on 1/16/18.
  */
sealed trait MyTry[+L]

case class Success[L](s: L) extends MyTry[L]

case class Failure[T <: Nothing](exception: Throwable) extends MyTry[T]

object MyTry {
  def apply[L](x: L): MyTry[L] = {
    try {
      Success(x)
    } catch {
      case NonFatal(e) => Failure(e)
    }
  }
}