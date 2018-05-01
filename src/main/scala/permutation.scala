/**
  * Created by abagla on 4/26/18.
  */

object test123 extends App {

  def permute(xs: List[Int]): List[List[Int]] = {
    def loop(x: Int): List[List[Int]] = {
      if (x == 0) List(List())
      else {
        for {
          per <- loop(x -1)
          value <- xs
          if !per.contains(value)
        } yield value +: per
      }
    }
    loop(xs.length)
  }

  println(permute(List(1,2,3)))
}
