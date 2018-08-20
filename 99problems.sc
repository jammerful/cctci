object binarytrees {

  sealed abstract class Tree[+T]
  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString =
      "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  }
  case object End extends Tree[Nothing] {
    override def toString = "."
  }
  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  // P55
  def cBalanced[T](n: Int, value: T): List[Tree[T]] = n match {
    case m if m <= 1 => List(End)
    case m if m % 2 == 0 => {
      val lesserSubtrees = cBalanced((n - 1) / 2, value)
      val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l =>
        greaterSubtrees.flatMap(g =>
          List(Node(value, l, g), Node(value, g, l))))
    }
    case m if m % 2 == 1 => {
      val subtrees = cBalanced((n - 1) / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    }
  }

  cBalanced(4, "x")

  // P61
  def leafCount[T](tree: Tree[T]): Int = tree match {
    case End               => 0
    case Node(v, End, End) => 1
    case Node(v, l, r)     => leafCount(l) + leafCount(r)
  }

  /*
        1
       / \
      2   6
     /
    3
   / \
  4   5
   */
  val testTree = Node(1,
                      Node(2, Node(3, Node(4, End, End), Node(5, End, End))),
                      Node(6, End, End))
  leafCount(testTree)

  // P61A
  def leafList[T](tree: Tree[T]): List[T] = tree match {
    case End               => List[T]()
    case Node(v, End, End) => List(v)
    case Node(v, l, r)     => leafList(l) ++ leafList(r)
  }

  leafList(testTree)

  // P62
  def internalNodes[T](tree: Tree[T]): List[T] = tree match {
    case End               => List[T]()
    case Node(v, End, End) => List[T]()
    case Node(v, l, r)     => List(v) ++ internalNodes(l) ++ internalNodes(r)
  }

  val testTree2 = Node('a', Node('b'), Node('c', Node('d'), Node('e')))
  internalNodes(testTree2)

  // P68
  def preOrder[T](tree: Tree[T]): List[T] = tree match {
    case End           => List()
    case Node(v, l, r) => List(v) ++ inOrder(l) ++ inOrder(r)
  }

  preOrder(testTree2)

  def inOrder[T](tree: Tree[T]): List[T] = tree match {
    case End           => List()
    case Node(v, l, r) => preOrder(l) ++ List(v) ++ preOrder(r)
  }

  inOrder(testTree2)

  def postOrder[T](tree: Tree[T]): List[T] = tree match {
    case End           => List()
    case Node(v, l, r) => postOrder(l) ++ postOrder(r) ++ List(v)
  }

  postOrder(testTree2)

  /////// Multi-Trees ///////////

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())

    override def toString =
      "M(" + value.toString + " {" + children
        .map(_.toString)
        .mkString(",") + "})"

    // P70C
    def nodeCount: Int = 1 + children.foldLeft(0)(_ + _.nodeCount)

    // P71
    def internalPathLength: Int =
      children.foldLeft(0)((acc, c) => acc + c.nodeCount + c.internalPathLength)

    // P72
    def postOrder: List[T] = children.flatMap(_.postOrder) ++ List(value)
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())
    def apply[T](value: T, children: List[MTree[T]]) =
      new MTree(value, children)
  }

  MTree('a', List(MTree('f'))).nodeCount
  val testMultiTree = MTree('a',
                            List(MTree('f', List(MTree('g'))),
                                 MTree('c'),
                                 MTree('b', List(MTree('d'), MTree('e')))))
  testMultiTree.internalPathLength
  testMultiTree.postOrder

  /*
  ///////////////// Graphs ////////////////////
   */
  abstract class GraphBase[T, U] {
    case class Edge(n1: Node, n2: Node, value: U) {
      def toTuple = (n1.value, n2.value, value)
    }
    case class Node(value: T) {
      var adj: List[Edge] = Nil
      // neighbors are all nodes adjacent to this node.
      def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
    }

    var nodes: Map[T, Node] = Map()
    var edges: List[Edge] = Nil

    // If the edge E connects N to another node, returns the other node,
    // otherwise returns None.
    def edgeTarget(e: Edge, n: Node): Option[Node]

//    override def equals(o: Any) = o match {
//      case g: GraphBase[_, _] =>
//        (nodes.keys.toList -- g.nodes.keys.toList == Nil &&
//          edges.map(_.toTuple) -- g.edges.map(_.toTuple) == Nil)
//      case _ => false
//    }

    def addNode(value: T) = {
      val n = new Node(value)
      nodes = Map(value -> n) ++ nodes
      n
    }

    def toTermForm: (List[T], List[(T, T, U)]) =
      (nodes.keys.toList, edges.map((e) => (e.n1.value, e.n2.value, e.value)))

    def toAdjacentForm: List[(T, List[(T, U)])] =
      nodes.values.toList.map((n) => (n.value, n.adj.map((e) =>
        (edgeTarget(e, n).get.value, e.value))))

    // P81
    def findPaths(start: T, end: T): List[List[T]] = {
      def findPathsR(curNode: Node, curPath: List[T]): List[List[T]] = {
        if (curNode.value == end) List(curPath)
        else curNode.adj.map(edgeTarget(_, curNode).get).filter(n => !curPath.contains(n.value)).flatMap(n => findPathsR(n, n.value :: curPath))
      }
      findPathsR(nodes(start), List(start)).map(_.reverse)
    }

    // P82
    def findCycles(start: T): List[List[T]] = {
      val n = nodes(start)
      n.adj.map(edgeTarget(_, n).get.value).flatMap(findPaths(_, start)).map(start :: _).filter(_.lengthCompare(3) > 0)
    }
  }

  class Graph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
      case g: Graph[_, _] => super.equals(g)
      case _              => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else if (e.n2 == n) Some(e.n1)
      else None

    def addEdge(n1: T, n2: T, value: U) = {
      val e = new Edge(nodes(n1), nodes(n2), value)
      edges = e :: edges
      nodes(n1).adj = e :: nodes(n1).adj
      nodes(n2).adj = e :: nodes(n2).adj
    }
  }

  class Digraph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
      case g: Digraph[_, _] => super.equals(g)
      case _                => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else None

    def addArc(source: T, dest: T, value: U) = {
      val e = new Edge(nodes(source), nodes(dest), value)
      edges = e :: edges
      nodes(source).adj = e :: nodes(source).adj
    }
  }

  abstract class GraphObjBase {
    type GraphClass[T, U]
    def addLabel[T](edges: List[(T, T)]) =
      edges.map(v => (v._1, v._2, ()))
    def term[T](nodes: List[T], edges: List[(T, T)]) =
      termLabel(nodes, addLabel(edges))
    def termLabel[T, U](nodes: List[T],
                        edges: List[(T, T, U)]): GraphClass[T, U]
    def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
      nodes.map(a => (a._1, a._2.map((_, ()))))
    def adjacent[T](nodes: List[(T, List[T])]) =
      adjacentLabel(addAdjacentLabel(nodes))
    def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]): GraphClass[T, U]
  }

  object Graph extends GraphObjBase {
    type GraphClass[T, U] = Graph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
      val g = new Graph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addEdge(v._1, v._2, v._3))
      g
    }
    def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
      val g = new Graph[T, U]
      for ((v, a) <- nodes) g.addNode(v)
      for ((n1, a) <- nodes; (n2, l) <- a) {
        if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
          g.addEdge(n1, n2, l)
      }
      g
    }
  }

  object Digraph extends GraphObjBase {
    type GraphClass[T, U] = Digraph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
      val g = new Digraph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addArc(v._1, v._2, v._3))
      g
    }
    def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
      val g = new Digraph[T, U]
      for ((n, a) <- nodes) g.addNode(n)
      for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
      g
    }
  }

  val testDigraph = Digraph.adjacentLabel(List(("m",List(("q",7))), ("p",List(("m",5), ("q",9))), ("k",List()), ("q",List())))
  testDigraph.findPaths("p", "q")
  testDigraph.findPaths("p", "k")

  val testGraph = Graph.term(List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
    List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h')))
  testGraph.findCycles('f')
}
