import scala.io.Source

/**
  * Created by abagla on 3/15/18.
  */
case class Vertex(value: Int, edges: List[Vertex])

class MyGraph[T](val graphMap: Map[T, List[T]]) {
  type Vertex = T

  def bfs(ver: Vertex): List[Vertex] = {
    def bfs_inner(queue: List[Vertex], visited: Set[Vertex]): List[Vertex] = {
      queue match {
        case Nil => List.empty
        case vertex :: tail if !visited.contains(vertex) =>
          vertex +: bfs_inner(tail ++ graphMap.getOrElse(vertex, List.empty), visited + vertex)
        case _ :: tail =>
          bfs_inner(tail, visited)
      }
    }
    bfs_inner(List(ver), Set.empty)
  }


  def dfs(ver: Vertex): List[Vertex] = {
    def dfs_inner(vertex: Vertex, visited: Set[Vertex]): List[Vertex] = {
      vertex :: graphMap.get(vertex).fold(List.empty[Vertex])
      { edges =>
        edges.foldLeft(List.empty[Vertex]) {
            case (acc, neigh) if !visited.contains(neigh) => dfs_inner(neigh, visited + neigh)
            case (acc, _) => acc
          }
      }
    }
    dfs_inner(ver, Set(ver))
  }

}




object Test {

//  def bfs(ver: Vertex): List[Vertex] = {
//    def bfs_inner(queue: List[Vertex], visited: Set[Vertex]): List[Vertex] = {
//      if (queue.isEmpty){
//        List.empty
//      } else {
//        if (!visited.contains(queue.head)) {
//          queue.head +: bfs_inner(queue.tail ++ queue.head.edges, visited + queue.head)
//        }
//        else {
//          bfs_inner(queue.tail, visited)
//        }
//      }
//    }
//    bfs_inner(List(ver), Set.empty)
//  }
//
//  def bfs2(ver: Vertex): List[Vertex] = {
//    def bfs_inner(queue: List[Vertex], visited: Set[Vertex]): List[Vertex] = {
//      queue match {
//        case Nil => List.empty
//        case vertex :: tail if !visited.contains(vertex) =>
//          vertex +: bfs_inner(tail ++ queue.head.edges, visited + queue.head)
//        case _ :: tail =>
//          bfs_inner(tail, visited)
//      }
//
//    }
//    bfs_inner(List(ver), Set.empty)
//  }
//
//  def dfs(ver: Vertex, currentSet: List[Vertex]): List[Vertex] = {
//    ver :: ver.edges.foldLeft(currentSet) {
//      case (acc, curr) =>
//         if (acc.contains(curr)) acc else {
//           dfs(curr, currentSet :+ curr)
//         }
//    }
//  }

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    val x = new MyGraph(Map(1 -> List(2, 4), 2 -> List(1, 3), 3 -> List(2, 4), 4 -> List(1, 3)))

    println(x.bfs(1))
    println(x.dfs(4))

    val fil = Source.fromFile(args(0)).getLines()
//    fil.map { x => s"${args(1)}/$x.jpg" }.foreach { x => new java.io.File(x).mo }

  }
}
