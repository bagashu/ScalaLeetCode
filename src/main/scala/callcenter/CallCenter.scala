package callcenter
import callcenter.{CallCompleted, GetCall}
import akka.actor.{Actor, ActorRef, PoisonPill, Props, ReceiveTimeout, Stash, Terminated, FSM}

object Employee1 extends Enumeration {
  type Employee1 = Value
  val Oper = Value
  val Super = Value
  val dir =  Value
}

object CallCenter {
  sealed trait Data
  case object EmptyDate extends Data

  case object InitData
  case object ReadyData
  sealed trait States
  case object Init extends States
  case object Ready extends States
  case object Finished extends States
}

class CallCenter(emp: Map[Employee1.Value, Int]) extends FSM[CallCenter.States, CallCenter.Data]{
  import CallCenter._
  startWith(Init, EmptyDate)
  self ! InitData
  when (Init) {
    case Event(InitData, _) =>
      self ! ReadyData
      goto(Ready)
  }
  when(Ready) {
    case Event(ReadyData, data) =>
      stay
  }
}
