/**
  * Created by abagla on 4/30/18.
  */

object Solution12 extends App{
  def solveSudoku(n: List[List[Int]]): Set[List[List[Int]]] = {

    def isValidSudoku(board: List[List[Int]]): Boolean = {
      val boxSize = Math.sqrt(board.length).toInt
      val tt = for {
        i <- board.indices
        j <- board.head.indices
        b1 <- i - (i% boxSize) until  i - (i% boxSize) + boxSize
        b2 <- j - (j% boxSize) until  j - (j% boxSize) + boxSize
        if board(i)(j) != 0 && b1 != i && b2 !=j
      } yield {
        board(b1)(b2) != board(i)(j)
      }

      !tt.contains(false) && board.forall(x => x.filterNot(_ == 0).distinct.length == x.filterNot(_ == 0).length) &&
        board.transpose.forall(x => x.filterNot(_ == 0).distinct.length == x.filterNot(_ == 0).length)
    }


    def isSafe(bo: List[List[Int]], r: Int, c: Int, value: Int): Boolean = {
      if(bo(r)(c) != 0) true else {
        isValidSudoku(bo.updated(r, bo(r).updated(c, value)))
      }
    }


    def subSolveSudoku(board: List[List[Int]], row: Int): Set[List[List[Int]]] = {
      if (row == -1) Set(board)
      else {
        for {
          b1 <- subSolveSudoku(board, row - 1)
          col <- n.indices
          value <- 1 to n.length
          if isSafe(b1, row, col, value)
        } yield {
          if (b1(row)(col) == 0) {
            println(s"\nboard right before is $b1, $row, $col, $value \n")
            val b2 = b1.updated(row, board(row).updated(col, value))
            println(s"\nboard right after is $b2, $row, $col, $value \n")
            b2
          }
          else b1
        }
      }
    }
    subSolveSudoku(n, n.length-1)
  }

  import scala.{List => $}

  val board1 = $(                        //0s denote empty cells
    $(1, 2, 0, 4),
    $(4, 0, 2, 1),
    $(2, 1, 4, 3),
    $(3, 4, 0, 2)
  )

  val board = $(                        //0s denote empty cells
    $(1, 0, 0, 0, 0, 7, 0, 9, 0),
    $(0, 3, 0, 0, 2, 0, 0, 0, 8),
    $(0, 0, 9, 6, 0, 0, 5, 0, 0),
    $(0, 0, 5, 3, 0, 0, 9, 0, 0),
    $(0, 1, 0, 0, 8, 0, 0, 0, 2),
    $(6, 0, 0, 0, 0, 4, 0, 0, 0),
    $(3, 0, 0, 0, 0, 0, 0, 1, 0),
    $(0, 4, 0, 0, 0, 0, 0, 0, 7),
    $(0, 0, 7, 0, 0, 0, 3, 0, 0)
  )
  println(solveSudoku(board1))
}