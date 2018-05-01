package meetup

/**
  * Created by abagla on 3/17/18.
  */
class BitoperationArray {

}

object BitoperationArray extends App {
  def bitFlip(input: Array[Int], oper: List[(Int, Int)]): Array[Int] = {
    val arr = Array.fill(input.length)(1)
    println(arr.toList)
    oper.foreach{
      case (i1, i2) => (i1 to i2).foreach{x => arr(x) = arr(x)* -1}
    }
    arr.zipWithIndex.foreach{
      case (elemnt, index) => if (elemnt == -1) {input(index) = input(index)^1}}
    input
  }
  val inp = Array(0,1,0,0,1)
  val operations= List((1,2), (2,4))

  println(bitFlip(inp, operations).toList)
}
