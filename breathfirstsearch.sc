object breathfirstsearch {

  class Graph {
    //Returns a set of vertices
    var vertices = Set[Vertex]()
    //Maintains a list of edges
    var edges = List[Edge]()
    //Maintains a map of vertices where each vertex(key) is connected to a list of vertices(value)
    var vertexMap = Map[Vertex, List[Vertex]]()

    def addVertex(v: Vertex) = vertices = vertices + v

    def addEdge(e: Edge) = {
      edges = e :: edges
      vertexMap += e.node1 -> (e.node2 :: vertexMap.getOrElse(e.node1, Nil))
    }

    override def toString = vertexMap.toString
  }

  case class Vertex(id: String, weight: Float = 0) {
    override def toString = id
  }

  case class Edge(node1: Vertex, node2: Vertex, weight: Float = 0)

  def bfs(g: Graph,
          unmarkedSet: Set[Vertex],
          queue: List[Vertex]): List[Vertex] = queue match {
    case Nil => Nil
    case head :: tail =>
      head :: bfs(g,
                  unmarkedSet - head,
                  tail ++ g.vertexMap
                    .getOrElse(head, List())
                    .reverse intersect unmarkedSet.toList)
  }
}
