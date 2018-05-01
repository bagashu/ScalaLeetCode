import org.scalatest.{FlatSpec, Matchers}

def ex(in: String): String = in


class test extends FlatSpec with Matchers {

  behavior of "testing"
  it should "" in {
    ex("123") should be ("123")
  }
}


case object daysofWeek extends Enumeration {
  val Monday = Value
  val Tuesday = Value
}

val x : daysofWeek.Value = daysofWeek.Monday


val p1 = List(Some(3), None).flatten.reduce(_ + _)

println(p1)



