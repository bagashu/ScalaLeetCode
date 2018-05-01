import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Created by abagla on 3/8/18.
  */
class Rational(val num: Int, val den: Int) {

  def add(that: Rational): Rational = {
    new Rational(num + that.num, den + that.den)
  }

  def neg() = {
    new Rational(-num, den)
  }
  def sub(that: Rational) = {
    add(that.neg)
  }

}

class Animal {
  val x = "hi"
  def sound = "rustle"
  def say: String = this.x
  def get = this.x
}


class Bird extends Animal{
  override val x = "bi"
  override def sound = "rustle"
  def say2 = "saying 2"

}

class Chicken extends Bird{
  override val x = "ci"
  override def sound = "rustle"
  def say3 = "saying 3"

}

object MyTest extends App {

  implicit class printer[T <: Animal](T: Animal) {
    def printMe(): Unit= {
      println(T.say)
     }
   }

  def covar(x: Animal) = {
    println(x.say)
  }

  def covar2(x: Chicken) = {
    println(x.say3)
  }

  val a1 = new Animal
  val b1 = new Bird
  val c1 = new Chicken

  a1.printMe
  b1.printMe
  c1.printMe

  val x = List(1,2,4)
  val y = List(4)
  println(x.zip(y))

  def multiply(m: Int)(n: Int): Int = m * n

  def mul = multiply(2)_

  println(mul(3))

  val bi = new Bird
  println(bi.say)
  covar(bi)
//  covar2(bi)

  def someFunc(x: Unit): Unit = {
    Future {
      throw new IllegalArgumentException("")
    }.recover{case e: Throwable =>
      println("in some func")
      throw e
    }
  }

//  val x1 = Try {
//    someFunc(x)
//  }
//    .recover{case e: Throwable => println("hello")}
//    .get
//
//  println(x1)
  Thread.sleep(1000)

  val p1 = "abcde"
  val p2 = "cdeab"

  def rotateString(A: String, B: String): Boolean = {
    if(A == B) return true
    if (A.length != B.length) return false
    B.foldLeft((A, false)){
      case((acc, flag), char) => val newAcc = acc.drop(1) + char
        if(newAcc == A)  (newAcc, true) else  (newAcc, flag)
    }._2
  }
  println(rotateString(p1,p2))

  val p4 = 1029
  println(Integer.parseInt(p4.toBinaryString, 2))

  def subsets(li: List[Int]): List[List[Int]] = {
    val t= li.foldLeft(List.empty[List[Int]]){
      case (acc, number) => acc ++ List(List(number)) ++ acc.map(x => x :+ number)
    }
    t.distinct
  }

  println(subsets(List(1,2,4)))

  def subsetsWithDup(nums: Array[Int]): List[List[Int]] = {
    (nums.foldLeft(List.empty[List[Int]]){
      case (acc, number) =>
        acc ++ List(List(number)) ++
          acc.map{x =>
            val tt = (x :+ number).sorted
            if (!acc.contains(tt)) tt else List.empty[Int]
          }
    } ::: List(List.empty[Int])).distinct
  }

  println(subsetsWithDup(Array(1,2,4)))

  def permute(li: Array[Int]): Array[List[Int]] = {
    var result: Array[List[Int]] = Array()
    def permute_inside(ints: Array[Int], l: Int, r:Int): Unit = {
      if(l == r) {
        println(ints.toList)
         result = result :+ ints.toList
      } else {
        (l to r).map { i =>
          var temp = ints(l)
          ints(l) = ints(i)
          ints(i) = temp
          permute_inside(ints, l+1, r)
          temp = ints(l)
          ints(l) = ints(i)
          ints(i) = temp
        }
      }
    }

    permute_inside(li, 0, li.length-1)
    result
  }

  def permuteFunction(li: List[Int]): List[List[Int]] = {
    val xx = for {
      ele <- li
      x <-li.indices
    } yield {
      val t= permuteFunction(li.filterNot(_ == ele))
      println(s"t is $t")
      t.map{x1=> ele +: x1}

    }
    xx.flatten
  }

  println("permutation is")
  println(permuteFunction(List(1,2,3)))

  def add(one: Option[Int], two: Option[Int]): Option[Int] = {
    (one ++ two).reduceOption(_ + _)
    one.fold(two){x => two.fold(one)(y => Some(x+y))}
  }
  println(add(None,Some(3)))
  println(add(Some(2),Some(3)))
  println(add(Some(2),None))
  println(add(None,None))


  val p3 = List(List(1,4), List(2,5))
  val p44 = List((1,4), (2,5))
  val p55 = (1,4)


  //leetcode 22 all valid parenthesis

  def generateParenthesis(n: Int): List[String] = {
    (1 to n).foldLeft(List("")){
      case (acc, _) =>
        acc.flatMap{str =>
          (0 to str.length)
            .map{index =>
              val (first, second)= str.splitAt(index)
              first + "()" + second } }.distinct
    }

  }
  println(generateParenthesis(2))


  //leetcode 167 2sum
  def twoSum2(nums: Array[Int], target: Int): Array[Int] = {
    val ma = nums.zipWithIndex.toMap
    nums
      .zipWithIndex
      .flatMap{ case (x2, index) => if(ma.contains(target - x2)) Some(index, ma(target- x2)) else None
    }.flatMap (x => List(x._1, x._2))
  }

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    var result = Array.empty[Int]
    nums.zipWithIndex.foldLeft(Map.empty[Int, Int]){
      case (acc, (element, index)) =>
        acc.get(target - element).fold(acc ++ Map(element -> index)){
          y =>
          result =   result.:+(y).:+(index)
            acc ++ Map(element -> index)
        }
    }
   result
  }
  val p2sum = Array(2, 7, 11, 15)
  println(twoSum(p2sum, 9).toList)


  // leet 11 containers
  def maxArea(height: Array[Int]): Int = {
    var result = List.empty[Int]
    height.zipWithIndex.tail.foldLeft(List((height.head, 0))){
      case (acc, (element, index)) =>
        result = result :+ acc.map {x=> (element min x._1) * (index - x._2) }.max
        acc :+ (element, index)
    }
    result.max
  }
  println(maxArea(Array(4,6,2,5)))



  // leet 807 Max Increase to Keep City Skyline
  def maxIncreaseKeepingSkyline(grid: Array[Array[Int]]): Int = {
    val side = grid.map(_.max)
    val top = grid.transpose.map(_.max)
    val t = for {
      i <- grid.indices
      j <- grid.head.indices
    } yield {
      List(top(i), side(j)).min - grid(i)(j)
    }
    t.sum
    }

  println(s"maxIncreaseKeepingSkyline is " +
    s"${maxIncreaseKeepingSkyline(Array(
      Array(3, 0, 8, 4),
      Array(2, 4, 5, 7),
      Array(9, 2, 6, 3),
      Array(0, 3, 1, 0)))}")



  // leetcode: validate suduko
  def isValidSudoku(board: Array[Array[String]]): Boolean = {
    val boxSize = Math.sqrt(board.length).toInt
    val tt = for {
      i <- board.indices
      j <- board.head.indices
      b1 <- i - (i% boxSize) until  i - (i% boxSize) + boxSize
      b2 <- j - (j% boxSize) until  j - (j% boxSize) + boxSize
      if board(i)(j) != "." && b1 != i && b2 !=j
    } yield {

      val rowCheck = board(i).filterNot(_ == ".")
      val colCheck = board.map(x => x(j)).filterNot(_ == ".")
      rowCheck.distinct.length == rowCheck.length &&
        colCheck.distinct.length == colCheck.length &&
        board(b1)(b2) != board(i)(j)
    }

    !tt.contains(false)
  }

  def isValidSudoku2(board: Array[Array[String]]): Boolean = {
    val boxSize = Math.sqrt(board.length).toInt
    val tt = for {
      i <- board.indices
      j <- board.head.indices
      b1 <- i - (i% boxSize) until  i - (i% boxSize) + boxSize
      b2 <- j - (j% boxSize) until  j - (j% boxSize) + boxSize
      if board(i)(j) != "." && b1 != i && b2 !=j
    } yield {
      board(b1)(b2) != board(i)(j)
    }

    !tt.contains(false) && board.forall(x => x.filterNot(_ == ".").distinct.length == x.filterNot(_ == ".").length) &&
      board.transpose.forall(x => x.filterNot(_ == ".").distinct.length == x.filterNot(_ == ".").length)
  }

  val sudo =  Array(
     Array("8","3",".",".","7",".",".",".","."),
  Array("6",".",".","1","9","5",".",".","."),
  Array(".","9","8",".",".",".",".","6","."),
  Array("8",".",".",".","6",".",".",".","3"),
  Array("4",".",".","8",".","3",".",".","1"),
  Array("7",".",".",".","2",".",".",".","6"),
  Array(".","6",".",".",".",".","2","8","."),
  Array(".",".",".","4","1","9",".",".","5"),
  Array(".",".",".",".","8",".",".","7","9")
   )

  val sudo1 =  Array(
    Array("5","3",".",".","7",".",".",".","."),
    Array("6",".",".","1","9","5",".",".","."),
    Array(".","9","8",".",".",".",".","6","."),
    Array("8",".",".",".","6",".",".",".","3"),
    Array("4",".",".","8",".","3",".",".","1"),
    Array("7",".",".",".","2",".",".",".","6"),
    Array(".","6",".",".",".",".","2","8","."),
    Array(".",".",".","4","1","9",".",".","5"),
    Array(".",".",".",".","8",".",".","7","9")
  )
  println(s"isValidSudoku for ${isValidSudoku2(sudo)}"  )



  // leetcode 540. Single Element in a Sorted Array

  def singleNonDuplicate(nums: List[Int]): Int = {
    def recursion(low:Int, high:Int):Option[Int] = {
      (low+high)/2 match {
      case _ if high < low => None
      case _ if high == low => Some(nums(high))
      case mid if nums(mid - 1) != nums(mid) && nums(mid + 1) != nums(mid) => Some(nums(mid))
      case mid if nums(mid - 1) == nums(mid) && (mid - low) % 2 == 0 => recursion(low, mid)
      case mid if nums(mid + 1) == nums(mid) && (high - mid) % 2 == 0 => recursion(mid, high)
      case mid if nums(mid - 1) == nums(mid) && (mid - low) % 2 != 0 => recursion(mid + 1, high)
      case mid if nums(mid + 1) == nums(mid) && (high - mid) % 2 != 0 => recursion(low, mid - 1)
    }
    }
    recursion(0,nums.length - 1).get
  }
  List(0,1,1)
  println(s"singleNonDuplicate is ${singleNonDuplicate(List(1,1,2,2,4,4,5,5,9))}")
  println(s"singleNonDuplicate is ${singleNonDuplicate(List(0,1,1))}")


  def lookup(array: Array[String], item: String): Boolean = {
    val temp = for {
      ele <- array
      if ele == item
    } yield {
      true
    }

    temp.nonEmpty
  }

  def lookup_second[T](array: Array[T], item: T): Boolean = {
    array.foldLeft(false){
      case (acc, ele)=> if(ele == item) true else acc
    }
  }


  println(s"lookup_second ${lookup_second(Array("ele", "bye"), "bye1")}")

}
