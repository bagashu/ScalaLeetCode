/**
  * Created by abagla on 1/16/18.
  */
//sealed trait MyTree[+T] {
//  def isDefined: Boolean
//
//  def map[B](f: T => B): MyTree[B] = {
//    this match {
//      case TreeNode(z, l, r) => TreeNode(f(z), l.map(f), r.map(f))
//      case EmptyNode => EmptyNode
//    }
//  }
//
//}
//case class TreeNode[T](elem: T, left: MyTree[T], right: MyTree[T]) extends MyTree[T] {
//  override def isDefined: Boolean = true
//}
//
//case object EmptyNode extends MyTree[Nothing] {
//  override def isDefined: Boolean = false
//}




object Solution1 extends App {

  case class LeetNode(value: Int, left: LeetNode = null, right: LeetNode = null)

  def averageOfLevels(root: LeetNode): Array[Double] = {

    def bfs_internal(acc: List[Tuple2[LeetNode, Int]]): List[Tuple2[Int, Int]] = {
      acc match {
        case Nil => List.empty
        case ((null, level):: tail) => bfs_internal(tail)
        case ((node, level) :: tail) =>
          (node.value, level) :: bfs_internal(tail :+ (node.left, level+1) :+ (node.right, level+1))
      }

    }
    println(bfs_internal(List((root, 0))).groupBy{_._2}.mapValues{x => x.foldLeft(0){(acc, y) => acc + y._1}/x.size}.values.toList.reverse)
    Array(2)
  }

  val x = LeetNode(1, LeetNode(2, LeetNode(6), LeetNode(4)), LeetNode(3, LeetNode(11)))
  averageOfLevels(x)


}