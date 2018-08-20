import scala.collection.mutable

object depthfirstsearch {

  case class Vertex(name: Char, edges: Set[Vertex])

  def dfsMutableIterative(start: Vertex): Set[Vertex] = {
    var current = start
    val found = mutable.Set[Vertex]()
    val visited = mutable.Stack[Vertex](current)

    while (visited.nonEmpty) {
      current = visited.pop()
      if (!found.contains(current)) {
        found += current
        for (next <- current.edges) {
          visited.push(next)
        }
      }
    }
    found.toSet
  }

  val testGraph = Vertex('a', Set(Vertex('b', Set()), Vertex('c', Set(Vertex('e', Set(Vertex('d', Set(Vertex('c', Set()), Vertex('f', Set())))))))))

  dfsMutableIterative(testGraph)

  def dfsMutableRecursive(start: Vertex): Set[Vertex] = {
    val found = mutable.Set[Vertex]()

    def recurse(current: Vertex): Unit = {
      found += current

      for (next <- current.edges) {
        if (!found.contains(next)) recurse(next)
      }
    }

    recurse(start)
    found.toSet
  }

  dfsMutableRecursive(testGraph)

  assert(dfsMutableRecursive(testGraph) == dfsMutableRecursive(testGraph))

  def dfsFunctional(start: Vertex): Set[Vertex] = {
    def loop(current: Vertex, acc: List[Vertex]): Set[Vertex] = {
      (for (next <- current.edges if !acc.contains(next))
        yield loop(next, List(current) ++ acc)).flatten ++ Set(current)
    }

    loop(start, List())
  }

  assert(dfsMutableIterative(testGraph) == dfsFunctional(testGraph))
}
