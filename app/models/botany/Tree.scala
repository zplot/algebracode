package models.botany

import scala.language.implicitConversions


/*
TODO CanonicalForm
1. Asignar un identificador a cada nodo
2. Asignar un peso a cada nodo
3. Reordenar nodos por pesos
4. Devolver árbl ordenado
*/

case class Point(x: Int, y: Int) {
  override def toString = "(" + x ++ "," + y + ")"

}
case class Node(val father: Option[Node], pos: Point) {
  override def toString = pos.toString
}
case class Edge(pos1: Node, pos2: Node)
case class Draw(actualNode: Node, nodes: List[Node], edges: List[Edge])



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

    val root = Node(None, Point(0,1))

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
        val newY = actualY -1
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

    val tmp = stringAnalyze(s.toList, initialDraw)
    tmp._2

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
  var mod = 0
  var thread = 0
  var ancestor = this

  override def toString = "*" + children.map(_.toString + "^").mkString("")

  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Tree]
    if (that == null) false
    else Tree.orderTree(this).children == Tree.orderTree(that).children
  }

}











