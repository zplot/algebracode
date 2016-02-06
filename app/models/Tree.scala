package models

import scala.language.implicitConversions

// http://aperiodic.net/phil/scala/s-99/


case class Point(x: Int, y: Int) {
  override def toString = "(" + x ++ "," + y + ")"

}
case class Node(val father: Option[Node], pos: Point) {
  override def toString = pos.toString
}
case class Edge(pos1: Node, pos2: Node)
case class Draw(actualNode: Node, nodes: List[Node], edges: List[Edge]) {


}
case class PrintableDraw(nodes: List[Node], edges: List[Edge])

object DrawSettings {

  val factor = 40
  val shiftX = 50
  val shiftY = 50

  // For circles
  val r ="6"
  val stroke = "black"
  val strokeWidth = "1"
  val fill = "red"

  // For lines
  val lineStyle = "stroke:rgb(40,40,40);stroke-width:1"

}


// http://aperiodic.net/phil/scala/s-99/
object Tree {



  implicit def string2Tree(s: String): Tree = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    val tmp = splitChildStrings(1).map(string2Tree(_))
    Tree(tmp)
  }

  // Eats a string and drops a list of nodes and a list of edges
  def string2Draw(s: String): Draw = {

    // Let's use the canonicalForm of the Tree
    val tree = string2Tree(s)
    val canonicalTree = tree.canonicalForm
    val cs = canonicalTree.toString

    val root = Node(None, Point(0, 1))

    def show(x: Option[Node]) = x match {
      case Some(node) => node
      case None => root
    }

    val initialDraw = Draw(root, List[Node](), List[Edge]())

    def stringAnalyze(s: List[Char], dibujo: Draw): (List[Char], Draw) = s match {

      case Nil => (Nil, dibujo)
      case '*' :: xs => stringAnalyze(xs, newNode(dibujo))
      case '^' :: xs => stringAnalyze(xs, goUp(dibujo))

    }

    def newNode(dibujo: Draw): Draw = {

      def firstEmptyX: Int = {
        val actualX = dibujo.actualNode.pos.x
        val actualY = dibujo.actualNode.pos.y
        val newY = actualY - 1
        val tmp1 = dibujo.nodes.filter(node => node.pos.y == newY )
        val tmp2 = tmp1.map(node => node.pos.x) // List of xs
        val tmp3 = if (tmp2.isEmpty) 0 else tmp2.max + 1 // highest x
        tmp3 // next empty x
      }


      val newNode = Node(Some(dibujo.actualNode), Point(firstEmptyX, dibujo.actualNode.pos.y - 1))
      val newEdge = Edge(dibujo.actualNode, newNode)

      if (dibujo.actualNode.pos == Point(0, 1)) {
        Draw(newNode, newNode :: dibujo.nodes, dibujo.edges)
      } else {
        Draw(newNode, newNode :: dibujo.nodes, newEdge :: dibujo.edges)
      }


    }

    def goUp(dibujo: Draw): Draw = Draw(show(dibujo.actualNode.father), dibujo.nodes, dibujo.edges)

    val tmp = stringAnalyze(cs.toList, initialDraw)

    val init: Draw = tmp._2


    init
  }

  def scaleDraw(draw: Draw): PrintableDraw = {

    import DrawSettings._



    val nodes = draw.nodes
    val newNodes = nodes.map(node => Node(node.father, Point(node.pos.x * factor + shiftX, - node.pos.y * factor + shiftY)))
    val edges = draw.edges
    val newEdges = edges.map(edge => {



      val p1X = edge.pos1.pos.x
      val p1Y = edge.pos1.pos.y
      val p2X = edge.pos2.pos.x
      val p2Y = edge.pos2.pos.y

      val newp1X = p1X * factor + shiftX
      val newp1Y = - p1Y * factor + shiftY
      val newp2X = p2X * factor + shiftX
      val newp2Y = - p2Y * factor + shiftY
      val dummyNode = Node(None, Point(0, 1))

      // Removing lines inside circles

      val slope: Float = if ((newp2X - newp1X) > 1) {  // The edge is not vertical
        (newp2Y - newp1Y).toFloat / (newp2X - newp1X).toFloat
      } else {
        99999 // The edge is vertical
      }

      val sqrtOfOnePlusTg2betha = math.sqrt(1 + slope * slope).toFloat
      val deltaX: Float = r.toFloat / sqrtOfOnePlusTg2betha
      val deltaY: Float = r.toFloat * math.sqrt(1 - deltaX * deltaX / r.toFloat / r.toFloat).toFloat

      val defp1X = newp1X + deltaX
      val defp1Y = newp1Y + deltaY
      val defp2X = newp2X - deltaX
      val defp2Y = newp2Y - deltaY

      val defp1XInt = defp1X.toInt
      val defp1YInt = defp1Y.toInt
      val defp2XInt = defp2X.toInt
      val defp2YInt = defp2Y.toInt

      Edge(Node(Option(dummyNode), Point(defp1XInt, defp1YInt)), Node(Option(dummyNode), Point(defp2XInt, defp2YInt)))
      }
    )

    PrintableDraw(newNodes, newEdges)
  }



  def orderTree(t: Tree): Tree = {

    if (t.children != List())  {
      val tmp3 = t.children.map( x => orderTree(x))
      val tmp4 = Tree(tmp3)
      val tmp5 = tmp4.children.sortBy(_.weight).reverse
      val tmp6 = Tree(tmp5)
      tmp6
    } else t

  }

}



case class Tree(children: List[Tree]) {

  def weight: Int = children.foldLeft(1)(_ + _.weight)
  def canonicalForm = Tree.orderTree(this)

  override def toString = "*" + children.map(_.toString + "^").mkString("")

  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Tree]
    if (that == null) false
    else Tree.orderTree(this).children == Tree.orderTree(that).children
  }

}

















