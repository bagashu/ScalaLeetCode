/**
  * Created by abagla on 3/15/18.
  */
import org.scalatest._
//class Test extends  FlatSpec with Matchers {
//
//  behavior of "hello"
//
//  it should "test something" in {
//    "a" should be ("a")
//  }
//
//}

case class Color(col:String)

class SetSpec extends FlatSpec with Matchers {

  "An empty Set" should "have size 0" in {
    Set.empty.size === 0
    val x = Color("hello")
    x.col should be ("hello")
  }
}