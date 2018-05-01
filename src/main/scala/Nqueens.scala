/**
  * Created by abagla on 4/30/18.
  */

object Solution11 extends App{
  def solveNQueens(n: Int): Set[List[Int]] = {
    def isSafe(colNum: Int, queen: List[Int]): Boolean = {
      !queen.contains(colNum) && queen.zipWithIndex.forall{case (x,y)=> x+y != colNum + queen.length } &&
        queen.zipWithIndex.forall{case (x,y)=> y-x !=  queen.length - colNum }
    }

    def subSolveQueen(x: Int): Set[List[Int]] = {
      if (x == 0) Set(List())
      else {
        for {
          queen <- subSolveQueen(x-1)
          col <- 0 until n
          if isSafe(col, queen)
        } yield {
          println(s" queen is $queen $col")
          queen :+ col
        }

      }
    }
    subSolveQueen(n)
  }

  println(solveNQueens(4))
}