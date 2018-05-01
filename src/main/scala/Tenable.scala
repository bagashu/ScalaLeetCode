/**
  * Created by abagla on 4/18/18.
  */
import TenableTest.Userdetails

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait MyView {
  def getuser(str: String): Future[Userdetails]
  def getToken: (String, String) => scala.concurrent.Future[Option[String]]
}

trait MyModel {
    def prefer2(name: String): Future[String]
    def preference(det: Userdetails): Future[String]
}

trait MyController { self: MyModel with MyView =>
  def ans1(name: String, pass: String): Future[String] = {
    for {
      user <- getToken(name, pass)
      detail <- getuser(user.get)
      y <- prefer2(detail.uname)
      zz <- preference(detail)
    } yield {
      s"$y--> $zz"
    }
  }
}


class MainImpl extends MyView with MyModel with MyController {
  def getuser(str: String): Future[Userdetails] = {
    Future.successful(Userdetails("name", "pass", "hello"))
  }

  def getToken: (String, String) => scala.concurrent.Future[Option[String]] =
    (name, pass) => name match {
      case "aashish" => Future.successful(None)
      case _ => Future.successful(Some(name))
    }

  def prefer2(name: String): Future[String] = Future.successful("bye")

  def preference(det: Userdetails): Future[String] = {
    Future.successful("hii")
  }
}


object Testing {
  implicit class intger(in: Int) {
    def mystring: String = in.toString
  }
}



object TenableTest extends App {

  import Testing._
  3.mystring

  case class Userdetails(uname: String, password: String, userdata: String)


  def getToken: (String, String) => scala.concurrent.Future[Option[String]] =
     (name, pass) => name match {
       case "aashish" => Future.successful(None)
       case _ => Future.successful(Some(name))
     }

  def getuser(str: String): Future[Userdetails] = {
    Future.successful(Userdetails("name", "pass", "hello"))
  }

  def prefer2(name: String): Future[String] = Future.failed(new IllegalArgumentException("bye"))
//    Future.successful("bye")
  def preference(det: Userdetails): Future[String] = {
    Future.successful("hii")
  }


  def ans1(name: String, pass: String): Future[String] = {
     for {
       user <- getToken(name, pass)
       detail <- getuser(user.get)
       y <- prefer2(detail.uname)
       zz <- preference(detail)
     } yield {
       s"$y--> $zz"
     }
  }



  val x = Some(3)
  val y: Option[Int] = None
  y.map(identity)

  x.map(identity)
  def ans(name: String, pass: String): Future[String] = {
    getToken(name, pass).flatMap{
        case None => Future.successful("not founf")
        case Some(user) =>
          getuser(user)
            .flatMap{detail => prefer2(detail.uname)
              .flatMap{y=> preference(detail)
                .map{zz=> s"$y--> $zz"}}}
    }
  }
  ans("aashish1", "bagla")onComplete {
    case scala.util.Success(posts) => println(s"an is $posts")
    case scala.util.Failure(t) => println("An error has occured: " + t.getMessage)
  }
  Thread.sleep(1000)

}
