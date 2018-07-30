object graph {

  class Graph[T] {
    type Vertex = T
    // Adjacency List
    type GraphMap = Map[Vertex, Set[Vertex]]
    var g: GraphMap = Map()

    def DFSNonFunc(start: Vertex): List[Vertex] = {
      val stack = new scala.collection.mutable.Stack[T]
      var visited = new scala.collection.mutable.ListBuffer[T]
      stack.push(start)

      while (!stack.isEmpty) {
        val current = stack.pop()
        if (!visited.contains(current)) {
          visited += current
          g(current).foreach(x => stack.push(x))
        }
      }
      visited.toList
    }

    def DFS(start: Vertex): List[Vertex] = {

      def DFS0(v: Vertex, visited: List[Vertex]): List[Vertex] = {
        if (visited.contains(v))
          visited
        else {
          val unvisitedNeighbors = g(v).filterNot(visited.contains)
          unvisitedNeighbors.foldLeft(v :: visited)((b,a) => DFS0(a, b))
        }
      }

      DFS0(start, List()).reverse
    }

    def BFSNonFunc(start: Vertex): List[List[Vertex]] = {
      ???
    }

    def BFS(start: Vertex): List[List[Vertex]] = {
      def BFS0(elems: List[Vertex], visited: List[List[Vertex]]): List[List[Vertex]] = {
        val newNeighbors = elems.flatMap(g(_)).filterNot(visited.contains).distinct
        if (newNeighbors.isEmpty)
          visited
        else
          BFS0(newNeighbors, newNeighbors :: visited)
      }

      BFS0(List(start), List(List(start))).reverse
    }
  }

  var graph = new Graph[Int]
  /*
      1--2
      |  |
      4--3
   */
  graph.g = Map(1 -> Set(2,4), 2-> Set(1,3), 3-> Set(2,4), 4-> Set(1,3))

  graph.DFS(1)
  graph.DFS(2)

  graph.DFSNonFunc(1)
  graph.DFSNonFunc(2)

  // graph.BFS(1)

}