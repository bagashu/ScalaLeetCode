import scala.annotation.tailrec
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack


sealed trait MyTree[+T] {
  def isDefined: Boolean

  def map[B](f: T => B): MyTree[B] = {
    this match {
      case TreeNode(z, l, r) => TreeNode(f(z), l.map(f), r.map(f))
      case EmptyNode => EmptyNode
    }
  }

  def preorder(f: T => Unit) {
    this match {
      case TreeNode(node, l, r) => f(node)
        l.preorder(f)
        r.preorder(f)
      case EmptyNode => {}
    }
  }

  def postorder_nonRecur(f: T => Unit) {
    var st = this match {
      case TreeNode(node, l, r) =>
        List((this, false))
      case EmptyNode => List.empty
    }
    while(st.nonEmpty) {
      val (element, visited) = st.head
      st = st.tail
      (element, visited) match {
        case (_, true) =>
          println(element)
        case (TreeNode(x, l ,r), false) =>
          st = (l, false) :: st
          st = (r, false) :: st
          (element, true) :: st
        case (EmptyNode, false) => {}
          }
      }
    }

  def postorder(f: T => Unit) {
    this match {
      case TreeNode(node, l, r) =>
        l.postorder(f)
        r.postorder(f)
        f(node)
      case EmptyNode => {}
    }
  }

  def inorder(f: T => Unit) {
    this match {
      case TreeNode(node, l, r) =>
        l.inorder(f)
        f(node)
        r.inorder(f)
      case EmptyNode => {}
    }
  }

  def levelorder(f: T => Unit) {

     @tailrec
     def loVisit(ls: List[MyTree[T]]): Unit = ls match {
      case Nil => None
      case TreeNode(value, l, r) :: rest =>
        f(value)
        loVisit(rest :+ l :+ r)
      case EmptyNode :: rest => loVisit(rest)
    }

    loVisit(List(this))
  }

}
case class TreeNode[T](elem: T, left: MyTree[T] = EmptyNode, right: MyTree[T] = EmptyNode) extends MyTree[T] {
  override def isDefined: Boolean = true


}

case object EmptyNode extends MyTree[Nothing] {
  override def isDefined: Boolean = false
}

object Solution extends App {


  def averageOfLevels[T <: Int](node: MyTree[T])(implicit num: Numeric[Int]): Iterable[Float] = {
    var lis = List.empty[(T, Int)]
    @tailrec
    def loop(li: List[(MyTree[T], Int)]): Unit = li match {
      case Nil => List.empty
      case ele :: tail => ele._1 match {
        case TreeNode(element, left, right) =>
          lis = lis :+ (element, ele._2)
          loop(tail :+ (left, ele._2+1) :+ (right, ele._2+1))
        case EmptyNode =>
          loop(tail)
      }
    }
    loop(List((node, 1)))
    val t= lis.groupBy(_._2)
    val p = scala.collection.immutable.TreeMap(t.toArray:_*).mapValues(_.map(_._1))
    p.mapValues(y =>  y.sum[Int](num).toFloat/y.length).values
  }

  def averageOfLevels1(node: MyTree[Int])(implicit num: Numeric[Int]): List[Float] = {
    var lis = scala.collection.mutable.TreeMap.empty[Int, List[Int]]
    @tailrec
    def loop(li: List[(MyTree[Int], Int)]): Unit = li match {
      case Nil => List.empty
      case ele :: tail => ele._1 match {
        case TreeNode(element, left, right) =>
          if(lis.contains(ele._2)) {
            lis.update(ele._2, lis(ele._2) :+ element)
          } else {
            lis += (ele._2 -> List(element))
          }
          loop(tail :+ (left, ele._2+1) :+ (right, ele._2+1))
        case EmptyNode =>
          loop(tail)
      }
    }
    loop(List((node, 1)))
    lis.values.map{x => x.sum.toFloat/x.length}.toList

  }

  val myTree1 = TreeNode(22, TreeNode(11, TreeNode(3)), TreeNode(30, TreeNode(26, TreeNode(24), TreeNode(28)), TreeNode(66)))
  println(s" avg is ${averageOfLevels1(myTree1)}")
  //  def breadth_first_traverse[Node](node: Node, f: Node => Queue[Node]): Stream[Node] = {
  //    def recurse(q: Queue[Node]): Stream[Node] = {
  //      if (q.isEmpty) {
  //        Stream.Empty
  //      } else {
  //        val (node, tail) = q.dequeue
  //        node #:: recurse(tail ++ f(node))
  //      }
  //    }
  //
  //    node #:: recurse(Queue.empty ++ f(node))
  //  }

  def moveZeroes(nums: Array[Int]): Unit = {
    if (nums.length < 1) return
    var (i,j) = (-1,0)
    while(i<j  && j< nums.length){
      if (nums(j) == 0) {
        if(i == -1) i = j
        j += 1
      } else {
        if (i != -1) {
          val pair = (nums(i),nums(j)).swap
          nums(i) = pair._1
          nums(j) = pair._2
          i += 1
        }
        j += 1
      }
    }
  }
  val moveZ = Array(0,22,0,33,12)
  moveZeroes(moveZ)
  println(moveZ.toList)

  def findTheDifference(s: String, t: String): Char = {
    var map1 = s.groupBy(x => x).mapValues(x => x.length)
    t.foldLeft(List.empty[Char]){
      case (acc, char) => if (map1.contains(char)) {
        if (map1(char) == 1) {
          map1 -= char
        } else {
          map1 += (char -> (map1(char) - 1))
        }
        acc
      } else char :: acc
    }.head
  }

  def judgeCircle(moves: String): Boolean = {
    val ma = Map('U' -> (0, 1), 'D' -> (0, -1), 'L' -> (-1, 0), 'R' -> (1, 0))
    moves.foldLeft((0, 0)) {
      case (acc, move) =>
        val mov = ma(move)
        (mov._1 + acc._1, mov._2 + acc._2)
    } match {
      case (0, 0) => true
      case _ => false
    }
  }

  def arrayPairSum(nums: Array[Int]): Int = {
    nums.sorted.sliding(2, 2).foldLeft(0) { case (acc, num) => num.min + acc }
  }

  def findComplement(num: Int): Int = {
    val x = num.toBinaryString.map {
      case '0' => 1
      case '1' => 0
    }.mkString
    Integer.parseInt(x, 2)
  }

  def fizzBuzz(n: Int): List[String] = {
    (1 to n).map {
      case x if x % 15 == 0 => "FizzBuzz"
      case x if x % 5 == 0 => "Buzz"
      case x if x % 3 == 0 => "Fizz"
      case x => x.toString
    }.toList
  }


  //  println(Integer.highestOneBit(1))
  //
  //  println("Let's take LeetCode contest".split(" ").map(_.reverse).mkString(" "))
  //  println(findComplement(5))

  //  var x = Array(1,1)
  //  println(arrayPairSum(x))

  //  println(judgeCircle(x))

  val tree1 = TreeNode(22, TreeNode(11, TreeNode(3)), TreeNode(30, TreeNode(26, TreeNode(24), TreeNode(28)), TreeNode(66)))


  def countPrimeSetBits(L: Int, R: Int): Int = {
    def isPrime(n: Int): Boolean = {
      if (n < 1) false
      else if (n == 1) true
      else {
        !((2 until n - 1) exists (n % _ == 0))
      }
    }

    def count1s(n: Int): Int = {
      n.toBinaryString.count {
        case '1' => true
        case '0' => false
      }

    }

    (L until R + 1).count {
      x =>
        println(x)
        val ones = count1s(x)
        isPrime(ones)
    }
  }

  println(countPrimeSetBits(244, 269))

  val n = 2342

  //  def quickSort(li: List[Int]): List[Int] ={
  //    li match {
  //      case head :: Nil => List(head)
  //      case Nil => List()
  //      case head :: tail =>val (first, last)= tail.partition(_ < par.head)
  //    }
  //  }
  //  val par = List(5,3,4,6)

  //  println(par.tail.partition(_ < par.head))


  def findTarget[A](root: MyTree[A], k: A)(implicit num: Numeric[A]): Boolean = {
    var li = List.empty[A]
    var flag = false

    def bfs(rr: List[MyTree[A]]): Unit = {
      rr match {
        case Nil => {}
        case EmptyNode :: rest => bfs(rest)
        case TreeNode(x, l, r) :: rest =>
          if (li contains num.minus(k, x)) {
            flag = true
          }
          li = li :+ x
          bfs(rest :+ l :+ r)
      }
    }

    bfs(List(root))
    flag
  }

  println(s"find target is ${findTarget(tree1, 15)}")

  class Animal {
    val sound = "rustle"
  }

  class Bird extends Animal {
    override val sound = "call"
  }

  class Chicken extends Bird {
    override val sound = "cluck"
  }

  def biophony[T <: Animal](things: Seq[T]) = things map (_.sound)

  def calValue[A](value: MyTree[A])(implicit or: Numeric[A]): A = {
    value match {
      case EmptyNode => 0.asInstanceOf[A]
      case TreeNode(x, l, r) => or.plus(or.plus(x, calValue(l)), calValue(r))
    }
  }

  def convertBST[A](root: MyTree[A], acc: A)(implicit or: Numeric[A]): MyTree[A] = {
    root match {
      case EmptyNode => EmptyNode
      case TreeNode(x, l, r) =>
        val value = calValue(r)
        TreeNode(or.plus(or.plus(value, x), acc),
          convertBST(l, or.plus(or.plus(value, x), acc)),
          convertBST(r, acc))
    }
  }

  println(convertBST(tree1, 0))


  val p1 = Array(Array(1, 2), Array(1, 2))

  def func(a: Array[Int], b: Array[Int]): Array[Int] = {
    println(s"a ${a(0)}, b $b")
    Array(a(0) min b(0), a(1) min b(1))
  }

  val p2 = p1.reduceLeftOption(func).fold(List(0, 0)) {
    _.toList
  }
  println(p2)

  def titleToNumber(s: String): Int = {
    s.reverse.zipWithIndex.foldLeft(0) { case (acc, (item, index)) => acc + ((item - 'A') * scala.math.pow(26, index)).toInt }
  }

  def sumOfLeftLeaves[A](root: MyTree[A])(implicit num: Numeric[A]): A = {
    var sum: A = 0.asInstanceOf[A]

    def loop(tree: MyTree[A]): Unit = {
      tree match {
        case EmptyNode => {}
        case TreeNode(x1, l, r)=>  l match {
          case EmptyNode => {}
          case TreeNode(x2, left, right) => if ((left == EmptyNode) && (right == EmptyNode)) {
            sum = num.plus(sum,  x2)
          }
        }
          loop(l)
          loop(r)
      }
    }
    loop(root)
    sum
  }

  println(s"sumOfLeftLeaves is ${sumOfLeftLeaves(tree1)}")



  def findTilt[A](root: MyTree[A])(implicit num: Numeric[A]): A = {

    def loop(tree: MyTree[A]): (A, A) = {
      tree match {
        case EmptyNode => (0.asInstanceOf[A], 0.asInstanceOf[A])
        case TreeNode(x, l, r) if l == EmptyNode && r == EmptyNode => (0.asInstanceOf[A], x)
        case TreeNode(x, l, r) =>
          val left = loop(l)
          val right = loop(r)
          (num.plus(num.plus(left._1, right._1), num.abs(num.minus(left._2, right._2))), num.plus(left._2, num.plus(right._2, x)))
      }
    }
    loop(root)._1
  }

  println(s"findTilt is ${findTilt(tree1)}")


  def findDiameter[A](root: MyTree[A])(implicit num: Numeric[A]): A = {

    def loop(tree: MyTree[A]): (A, A) = {
      tree match {
        case EmptyNode => (0.asInstanceOf[A], 0.asInstanceOf[A])
        case TreeNode(x, l, r) if l == EmptyNode && r == EmptyNode => (1.asInstanceOf[A], 1.asInstanceOf[A])
        case TreeNode(x, l, r) =>
          val left = loop(l)
          val right = loop(r)

          (num.plus(num.plus(left._1, right._1), num.abs(num.minus(left._2, right._2))), num.plus(left._2, num.plus(right._2, x)))
      }
    }
    loop(root)._1
  }

  def findBottomLeftValue[A](root: MyTree[A]): A = {
    var li = List()
    var value: A = 0.asInstanceOf[A]
    def loop(list: List[MyTree[A]]): List[A] ={
      list match {
        case Nil => List.empty
        case TreeNode(x,l,r) :: tail =>
          value = l match {
            case EmptyNode=> value
            case TreeNode(x2,l2,r2) => x2
          }
          (li :+ x)  ::: loop(tail :+ l :+ r)
        case EmptyNode:: tail => loop(tail)
      }
    }
    root match {
      case TreeNode(x,l,r) => value = x
      case EmptyNode => 0.asInstanceOf[A]
    }
    loop(List(root))
    value
  }

  val tree2 = TreeNode(1, TreeNode(2, TreeNode(4)), TreeNode(3, TreeNode(5, TreeNode(7)), TreeNode(6)))
  println(s"findBottomLeftValue is ${findBottomLeftValue(tree2)}")


  val x1 =List(1)
  x1 match {
    case Nil => print("heelo")
    case y :: tail => print(s"$y and $tail")
    case t => print(t)

  }

  // leetcode 814. Binary Tree Pruning (Medium)
  def pruneTree[A](root: MyTree[A]): MyTree[A] = {
    root match {
      case EmptyNode => EmptyNode
      case TreeNode(value, l, r) if value == 1 => TreeNode(value, pruneTree(l), pruneTree(r))
      case re@TreeNode(value, l, r) =>
        val left =  pruneTree(l)
        val right = pruneTree(r)
        (left, right) match {
          case (EmptyNode, EmptyNode) => EmptyNode
          case (TreeNode(val1, le1, re1),TreeNode(val2, le2, re2)) if val1 ==0 && val2 == 0=> EmptyNode
          case (EmptyNode,TreeNode(val2, le2, re2)) if val2 == 0 => EmptyNode
          case (TreeNode(val1, le1, re1),EmptyNode) if val1 == 0 => EmptyNode
          case _ => TreeNode(value, left, right)
        }
    }
  }

  val tree4 = TreeNode(1, TreeNode(0, TreeNode(0), TreeNode(0)), TreeNode(1, TreeNode(0), TreeNode(1)))
  val tree5 = TreeNode(1, TreeNode(1, TreeNode(1, TreeNode(0)), TreeNode(1)), TreeNode(0, TreeNode(0), TreeNode(1)))

  println(s"pruneTree is ${pruneTree(tree5)}")

}
