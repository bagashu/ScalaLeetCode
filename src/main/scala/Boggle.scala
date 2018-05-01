/**
  * Created by abagla on 5/1/18.
  */

object Boggle extends App {


  def solveBoggle(charses: List[List[Char]], strings: List[String]): List[String] = {

    def isValid(i: Int, i1: Int): Boolean = {
      i > -1 && i < charses.length  && i1 > -1 && i1 < charses.transpose.length
    }

    def alreadyVisited(i: Int, i1: Int, ints: List[Int]): Boolean = {
      ints.contains(i*charses.length + i1)
    }

    def isSafe(xs: List[Int], letter: Int): Boolean = {
      if (xs.isEmpty) true
      else {
        val (row, col) = (xs.last/charses.length, xs.last%charses.length)
        val moves = List((-1,-1), (0, -1), (1, -1), (-1,0),(1,0),(-1,1),(0,1),(1,1))
        moves
          .filter{case (x,y) => isValid(row+x, col+y) && !alreadyVisited(row+x, col+y, xs)}
          .map{case (x,y) => ((row+x)*charses.length) + (col+y)}
          .contains(letter)
      }
    }

    def loop(xs: Int): List[List[Int]] = {
      if (xs == 0)  List(List.empty[Int])
      else {
        for {
          sols <- loop(xs -1)
          index <-  0 until charses.length * charses.transpose.length
          if !sols.contains(index) && isSafe(sols, index)
        } yield {
          println(s"sol is $sols, $index")
          sols :+ index
        }
      }
    }
    def convertString(li: List[Int]): String = {
      li.map(x => charses(x/charses.length)(x%charses.length)).mkString
    }
//    loop(charses.length * charses.transpose.length).map(convertString)
    loop(5).map(convertString)

  }



  val boggle = List(
    List('G','I','Z'),
    List('U','E','K'),
    List('Q','S','E'))

  val dict = List("GEEKS", "FOR", "QUIZ", "GO")

  println(solveBoggle(boggle, dict).filter(x => x.startsWith("GEEKS")))
}