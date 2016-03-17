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
object Node3 {

  implicit def string2Tree(s: String): Node3 = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    val tmp = splitChildStrings(1).map(string2Tree(_)).toVector
    Node3(tmp)
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
      case _ => (s, dibujo) // Este caso no se da nunca. Ponemos esto para evitar warnings

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

  def orderTree(t: Node3): Node3 = {

    if (t.children != List())  {
      val tmp3 = t.children.map( x => orderTree(x))
      val tmp4 = Node3(tmp3)
      val tmp5 = tmp4.children.sortBy(_.weight).reverse
      val tmp6 = Node3(tmp5)
      tmp6
    } else t

  }

}


case class Node3(children: Vector[Node3]) {

  val childrenNum = children.length

  def weight: Int = children.foldLeft(1)(_ + _.weight)

  def canonicalForm = Node3.orderTree(this)

  def isLeaf: Boolean = this.children == Vector[Node3]()

  def hasChildren: Boolean = ! isLeaf

  def numChildren = children.length

  var mod = 0
  var thread: Either[Int, Node3] = Left(0)
  var ancestor = this
  var prelim = 0
  var defaultAncestor = this
  var father: Node3 = this
  var leftSibling: Option[Node3] = None
  var leftMostSibling: Option[Node3] = None
  var leftMostChild: Option[Node3] = None
  var rightMostChild: Option[Node3] = None
  var shift: Int = 0



  override def toString = "*" + children.map(_.toString + "^").mkString("")

  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Node3]
    if (that == null) false
    else Node3.orderTree(this).children == Node3.orderTree(that).children
  }
}

object TreeLayaut {

  val distance = 10

  def layaut(t: Node3) = {
    initWalk(t)
    firstWalk(t)
    secondWalk(t)


  }


  def initWalk(n: Node3): Unit = {

    n.rightMostChild = n.numChildren match {
      case 0 => None
      case x => Some(n.children(x - 1))
    }
    n.leftMostChild = n.numChildren match {
      case 0 => None
      case x => Some(n.children(0))
    }
    for (t <- n.children) {
      t.father = n

      initWalk(t)
    }
    for (t <- n.children.indices) {
      if (t == 0) n.children(t).leftSibling = None else {
        n.children(t).leftSibling = Some(n.children(t - 1))
        n.children(t).leftMostSibling = Some(n.children(0))
      }
    }



  }

  def firstWalk(v: Node3): Unit = {
    if (v.isLeaf) {
      v.prelim = 0
    } else {
      v.defaultAncestor = v.children(0)
      for (w <- v.children) {
        firstWalk(w)
        apportion(w, w.defaultAncestor)
      }
      executeShifts(v)
      val midpoint = 1 / 2 * (v.children(0).prelim + v.children(v.childrenNum - 1).prelim)
      val tmp = v.leftSibling match {
        case None => midpoint
        case Some(w) => {
          v.prelim = w.prelim + distance
          v.mod = v.prelim - midpoint
        }
      }

    }
  }

  def secondWalk(v: Node3): Unit = {



  }

  def inTheBox(x: Option[Node3]): Node3 = x match {
    case None => Node3(Vector[Node3]())
    case Some(y) => y
  }

  def apportion(v: Node3, defaultAncestor: Node3): Unit = {

    val w = v.leftSibling match {
      case None => v
      case Some(w) => w
    }

    if (w != v) {

      var vInPlus: Node3 = v
      var vOutPlus: Node3 = v
      var vInMinus: Either[Int, Node3] = Right(w)
      var vOutMinus: Option[Node3] = vInPlus.leftMostSibling
      var sOutPlus: Int = vOutPlus.mod
      var sInPlus: Int = vInPlus.mod
      var sInMinus: Int = vInMinus.mod
      var sOutMinus: Int = inTheBox(vOutMinus).mod

      while ((nextRight(vInMinus) != Left(0)) && (nextLeft(vInPlus) != Left(0)) {

        vInMinus = nextRight(vInMinus)
        vInPlus = nextRight(vInPlus)
        vOutMinus = vOutMinus match {
          case None => None
          case Some(x) => Some(nextLeft(inTheBox(vOutMinus)))
        }
        vOutPlus = nextRight(vOutPlus)
        vOutPlus.ancestor = v
        v.shift = vInMinus.prelim + sInMinus - vInPlus.prelim - sInPlus + distance
        if (v.shift > 0) {
          moveSubtree(ancestor(vInMinus, v, v.defaultAncestor), v, v.shift)
          sInPlus = sInPlus + v.shift
          sOutPlus = sOutPlus + v.shift
        }
        sInMinus = sInMinus + vInMinus.mod
        sInPlus = sInPlus + vInPlus.mod
        sOutMinus = sOutMinus + inTheBox(vOutMinus).mod

        
      }

      def ancestor(w: Node3, v: Node3, d: Node3) = {

        if (vInMinus.ancestor.father == v ) {
          vInMinus.ancestor
        } else {
          v.defaultAncestor
        }
      }


    }

    def moveSubtree(wMinus: Node3, wPlus: Node3, shift: Int): Unit = {



    }




  }



  def executeShifts(v: Node3): Unit = {


  }

  def nextLeft(v: Node3): Either[Int, Node3] = {
    if (v.hasChildren) Right(inTheBox(v.leftMostChild)) else v.thread
  }


  def nextRight(v: Node3): Either[Int, Node3] = {
    if (v.hasChildren) Right(inTheBox(v.rightMostChild)) else v.thread
  }






}










































