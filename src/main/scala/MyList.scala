import scala.annotation.tailrec

/**
  * Created by abagla on 1/15/18.
  */

sealed trait MyList[+T] {

  def map[B](f: T => B): MyList[B] = {
    this match {
      case Node(head, tail) => Node(f(head), tail.map(f))
      case Empty => Empty
    }
  }

  @tailrec
  final def foldLeft[B](acc: B)(f: (B, T) => B): B = {
    this match {
      case Node(head, tail) =>
        val x = f(acc, head)
        tail.foldLeft(x)(f)
      case Empty => acc
    }
  }

  def foldRight[B](acc: B)(f: (T, B) => B): B = {
    this.reverse.foldLeft(acc)((acc,item) => f(item,acc))
  }

  def reverse: MyList[T] = {
    this.foldLeft(MyList[T]()){
      case (acc, item) => Node(item, acc)
    }
  }

  def ::[B >: T](elem: B): MyList[B] ={ Node(elem, this) }
}

case class Node[T](head: T, tail: MyList[T]) extends MyList[T]

case object Empty extends MyList[Nothing]

object MyList {
  def apply[T](items: T*): MyList[T] = {
    items match {
      case Nil => Empty
      case seq => Node(seq.head, apply(seq.tail: _*))
    }
  }
}




object MyListTest extends App {
  val x = MyList(1, 3, 5)
  x.map(println)
  println(x.foldLeft(1){ case(acc, num) => acc+num})
  println(x.reverse)
  println(12 :: x)
  val xx = Some(2)
  println(xx.flatMap(z=> None))
  println(xx.map(z=> None))
  val xxx = List(1,5,3)
  xxx.foldRight(1)((b,a) => b+ a)
  xxx.foldLeft( (xxx.head, xxx.tail.head))((r, c) => (r._2, c) )._1
  println(List(1,2,3,4).span(x => x % 2 != 0))


  def encode[A](li: List[A]): List[Tuple2[A, Int]] = {
    li.foldLeft(List[Tuple2[A, Int]]()){
      case (acc, item) =>
        acc.find(x => x._1 == item)
          .fold{Tuple2(item, 1) :: acc} { y =>
            acc.map { x => if (x._1 == item) (x._1, x._2 + 1) else x } }
    }
  }
  val t = List(1,1,1,1,3,4,4,4,1,5,3,4)
  println(encode(t))

  def findTheDifference(s: String, t: String): Unit = {

    val x = t.foldLeft((1 until 27).map(x=> 0).toList){
      case (acc , char) => acc.updated(char - 'a', acc(char - 'a')+1)
    }


    val p = s.foldLeft(x){
      case (acc, item) => acc.updated(item - 'a', acc(item - 'a')-1)
    }
      .zipWithIndex
      .map{
        case (1, index) => index + 'a'
        case (_, index) => 0
      }.filter(_ != 0).head.toChar

    println(x)
    println(p)
  }

  findTheDifference("a" , "aa")

  def moveZeroes(nums: Array[Int]): List[Int] = {
    val x = nums.foldLeft((List.empty[Int], 0)){
      case ((acc, index), item) => acc match {
        case Nil => (List(item), 0)
        case li if item == 0 => (acc :+ item, index)
        case li => (acc.updated(index, item) :+ 0, index +1)
      }
    }._1
    println(s"x is $x")
    x
  }
  println(moveZeroes(Array(0, 1, 0, 3, 12)))


  def maxProfit(prices: Array[Int]): Int = {
    var local = 0
    var global = 0
    prices.toList.sliding(2)
      .map{
        case x :: y :: Nil => y-x
        case x :: Nil => 0
        case _ => 0
      }.foreach{ x =>
      local = local + x
      if (local< 0) local =0
      if (global <= local ) global = local
    }
    global
  }

  println(s"max profit is ${maxProfit(Array(7,1,5,3,6,4))}")


  def mergeTwoLists[T](l1: MyList[T], l2: MyList[T])(implicit ord: T => Ordered[T]): MyList[T] = {
    (l1, l2) match {
      case (Node(head1, left), Node(head2, right)) if head1 <= head2 => Node(head1, mergeTwoLists(left, l2))
      case (Node(head1, left), Node(head2, right)) if head1 > head2 => Node(head2, mergeTwoLists(l1, right))
      case (Empty, Empty) => Empty
      case (Empty, right) => right
      case (left, Empty) => left
    }
  }

  println(mergeTwoLists(MyList(1, 3, 4), MyList(1,2,4)))

  // start from the second element,
  // slow pointer starts from first
  // for every distinct item in the list, increment slow pointer and copy it with the new item
  // i.e basically we are moving the distinct items forward
  def removeDuplicates(nums: Array[Int]): Int = {
    nums match {
      case Array() => 0
      case num =>

        var end = 0
        for {(t, index) <- nums.zipWithIndex
             if index != 0
             if nums(end) != nums(index)
        } {
          end = end + 1
          nums(end) = nums(index)
        }
        end + 1
    }
  }

  println(s"remove duplicates ${removeDuplicates(Array())}")


  def lengthOfLastWord(s: String): Int = {
    s.split(' ').lastOption.fold(0){_.length}
  }

  println(s"lenght of last ${lengthOfLastWord(" ")} ")

  def canPlaceFlowers(flowerbed: Array[Int], n: Int): Boolean = {
    var window = 1
    var nu = n
    flowerbed.zipWithIndex.foreach {
      case (1, _) => window = 0
      case (0, index) if window == 1 && index == flowerbed.length-1=>
        println(s"im in side nu os $nu")
        nu=nu-1
      case (0, _) =>
        window = window + 1
        if (window == 3){
          nu = nu -1
          window = 1
        }
    }
    if (nu <=0) true else false
  }

  println(s"canPlaceFlowers of last ${canPlaceFlowers(Array(1,0,0,0,1,0,0), 2)} ")



  val s1 = Set(1,2,4).subsets

  def findDuplicates(nums: Array[Int]): List[Int] = {
    var li = List.empty[Int]
    nums.foreach{x =>
      val tt = nums(x-1)
      if (tt > 0) nums.update(x-1, -tt) else
        li  = li :+ x
    }
    li
  }


  def swapPairs[A](head: MyList[A]): MyList[A] = {
    println(s"swap is $head")
    head match {
      case Empty => Empty
      case Node(x1, tail) if tail != Empty => Node(tail.asInstanceOf[Node[A]].head, Node(x1, swapPairs(tail.asInstanceOf[Node[A]].tail)))
      case Node(x1, tail) => Node(x1, Empty)
    }
  }


  val p1 = Node(1, Node(2, Node(3, Node(4, Empty))))
  println(s"swapPairs is ${swapPairs(p1)}")

//  val p2 = List((1,2), (2,3), (3,4), (1,3))
  val p2 = List((1,2),(2,3))
  val tt = p2.sortBy(x => x._2)(Ordering[Int])
  val x2 = tt.foldLeft(List(tt.head)){case (acc , item) => if (item._1 >= acc.last._2) acc :+ item else acc}
  println(s"x2 is $x2")


  def fibonnaci(n: Int): Int = {
    @tailrec
    def loop(d: Int, a: Int = 0, b:Int = 1): Int = {
      d match {
        case 0 => a
        case x1 => loop(x1 -1, b, a+b)
      }
    }
    loop(n)
  }


  def factorial(n: Int): Int = {
    @tailrec
    def loop(d: Int, a: Int = 1): Int = {
      d match {
        case 1 => a
        case x1 => loop(x1 -1, x1*a)
      }
    }
    loop(n)
  }

  println(s"factorial is ${factorial(4)}")

  val p3 = "asdfad"

  val grades = Map("Kim" -> 90,
       "Al" -> 85,
     "Melissa" -> 95,
     "Emily" -> 91,
     "Hannah" -> 92)

  val p4 = List( (7,1),(4,4),(7,0),(5,0),(6,1),(5,2))
  def compareFunc(a: (Int, Int), b: (Int, Int)): Boolean = {
    if (a._1 > b._1) true
    else if (a._1 == b._1) { if(a._2 < b._2) true else false}
    else false
  }

  val p5 = p4.sortWith(compareFunc)
  println(p5.foldLeft(List.empty[(Int,Int)]){case (acc, item) => val (front, back) = acc.splitAt(item._2)
  front ++ List(item) ++ back})
//    .foldLeft(List[(Int, Int)]){case (acc, item) => acc



}
