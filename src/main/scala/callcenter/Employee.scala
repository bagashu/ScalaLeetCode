package callcenter
import akka.actor.{Actor, ActorRef, PoisonPill, Props, ReceiveTimeout, Stash, Terminated}
import akka.actor.Actor.Receive
//sealed trait Employee extends Actor
trait Employee {
def id: String
}


case object GetCall
case object CallCompleted



case class Operator(id: String) extends Employee with Actor {
  override def receive: Receive = {
    case GetCall => doSomething
    case CallCompleted => context.parent forward(CallCompleted)
      self ! PoisonPill
  }

  def doSomething = {
    Thread.sleep(1000)
    self ! CallCompleted
  }
}


case class SuperVisor(id: String) extends Employee with Actor {
  override def receive: Receive = {
    case GetCall => doSomething
    case CallCompleted => context.parent forward(CallCompleted)
      self ! PoisonPill
  }

  def doSomething = {
    Thread.sleep(2000)
    self ! CallCompleted
  }
}

object tester extends App {
  println("eh")
}