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

case class Node(father: Option[Node], pos: Point) {
  override def toString = pos.toString
}

case class Edge(pos1: Node, pos2: Node)

case class Draw(actualNode: Node, nodes: List[Node], edges: List[Edge])



// http://aperiodic.net/phil/scala/s-99/
object Node3 {

  var lastId = 0

  def apply(children: Vector[Node3]): Node3 = {

    val newId = lastId + 1
    new Node3(newId, children)

  }

  def unapply(x: Node3): Option[Vector[Node3]] = Some(x.children)



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

}

object Utils {

  val nothing = Node3(Vector[Node3]())

  implicit def inTheBox(z: Option[Node3]): Node3 = z match {
    case None => nothing
    case Some(x) => x
  }

  implicit def inTheBoxFather(z: Option[Node3]): Node3 = z match {
    case None => nothing
    case Some(x) => x
  }

  def orderTree(t: Node3): Node3 = {

    if (t.children != Vector[Node3]())  {
      val tmp3 = t.children.map( x => orderTree(x))
      val tmp4 = Node3(tmp3)
      val tmp5 = tmp4.children.sortBy(_.weight).reverse
      val tmp6 = Node3(tmp5)
      tmp6
    } else t

  }

}

case class Tree3(root: Node3)

class Node3(val id: Int, val children: Vector[Node3]) {

  val childrenNum = children.length
  def weight: Int = children.foldLeft(1)(_ + _.weight)
  def canonicalForm = Utils.orderTree(this)
  def isLeaf: Boolean = this.children == Vector[Node3]()
  def hasChildren: Boolean = ! isLeaf
  def numChildren = children.length

  var mod: Double = 0
  var thread: Option[Node3] = None
  var ancestor: Option[Node3] = None
  var prelim: Double = 0
  var defaultAncestor: Option[Node3] = None
  var father: Option[Node3] = None // En initWalk
  var leftSibling: Option[Node3] = None // En initWalk
  var leftMostSibling: Option[Node3] = None // En initWalk
  val leftMostChild: Option[Node3] = if (isLeaf) None else Some(children(numChildren - 1))
  val rightMostChild: Option[Node3] = if (isLeaf) None else Some(children(0))
  var shift: Double = 0
  var x: Double = 0
  var y: Double = 0
  var yStep: Double = 10 // Paso de nivel y
  var level: Int = 0 // En initWalk

  var number: Int = -1  // en initWalh

  var subTrees: Int = 0
  var change: Double = 0


  // TODO Descomentar lo de abajo para imprimir * y ^
  override def toString = "*" + children.map(_.toString + "^").mkString("")

/*  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Node3]
    if (that == null) false
    else Node3.orderTree(this).children == Node3.orderTree(that).children
  }*/
}

object TreeLayaut {

  val distance = 10

  def layaut(t: Tree3): Unit = {

    initWalk(t)
    firstWalk(t.root)
    secondWalk(t.root, 0)  // TODO cuál es el segundo agumento?

  }

  def initWalk(tree: Tree3): Unit = {

    import Utils.inTheBoxFather
    val root = tree.root
    root.father = None
    root.level = 0
    root.leftSibling = None
    root.leftMostSibling = None
    root.number = -1
    initNextLevel(root)

    def initNextLevel(n: Node3): Unit = {

      for (t <- n.children) {
        t.father = Some(n)
        t.level = n.level + 1
        t.number = {
          val siblings: Vector[Node3] = t.father.children
          val mapa: Map[Node3, Int] = if (siblings == Vector()) Map() else siblings.zipWithIndex.toMap
          if (mapa == Map()) -100 else mapa(t)
        }
        import Utils.inTheBoxFather
        t.leftSibling = {
          if (t.number == 0) None else Some(t.father.children(t.number - 1))
        }
        t.leftMostSibling = {
          if (t.number == 0) None else Some(t.father.children(0))
        }
        initNextLevel(t)
      }
    }
  }

  def firstWalk(v: Node3): Unit = {
    import Utils.inTheBox
    if (v.isLeaf) {
      v.prelim = 0
    } else {
      v.defaultAncestor = Some(v.children(0))
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

  def secondWalk(v: Node3, m: Double): Unit = {

    v.x = v.prelim + m
    v.y =v.level
    for (w <- v.children) {
      secondWalk(w, m + v.mod)
    }
  }

  def apportion(v: Node3, defaultAncestor: Node3): Unit = {

    import Utils.inTheBox

    val w: Node3 = v.leftSibling match {
      case None => v
      case Some(a) => a
    }

    if (w != v) {

      var vInPlus: Option[Node3] = Some(v)
      var vOutPlus: Option[Node3] = Some(v)
      var vInMinus: Option[Node3] = Some(v)
      var vOutMinus: Option[Node3] = vInPlus.leftMostSibling
      var sOutPlus: Double = vOutPlus.mod
      var sInPlus: Double = vInPlus.mod
      var sInMinus: Double = vInMinus.mod
      var sOutMinus: Double = vOutMinus.mod


      while (nextRight(vInMinus).isDefined && nextLeft(vInPlus).isDefined) {

        vInMinus = nextRight(vInMinus)
        vInPlus = nextRight(vInPlus)
        vOutMinus = nextLeft(vOutMinus)
        vOutPlus = nextRight(vOutPlus)
        vOutPlus.ancestor = Some(v)
        v.shift = vInMinus.prelim + sInMinus - vInPlus.prelim - sInPlus + distance
        if (v.shift > 0) {
          moveSubtree(ancestor(inTheBox(vInMinus), v, v.defaultAncestor), v, v.shift)
          sInPlus = sInPlus + v.shift
          sOutPlus = sOutPlus + v.shift
        }
        sInMinus = sInMinus + vInMinus.mod
        sInPlus = sInPlus + vInPlus.mod
        sOutMinus = sOutMinus + vOutMinus.mod
        sOutPlus = sOutPlus + vOutPlus.mod
      }

      if (nextRight(vInMinus).isDefined && nextRight(vOutPlus).isEmpty) {

        vOutPlus = nextRight(vInMinus)
        vOutPlus.mod = vOutPlus.mod + sInPlus - sOutMinus

      }

      if (nextLeft(vInPlus).isDefined && nextLeft(vOutMinus).isEmpty) {

        vOutMinus.thread = nextLeft(vInPlus)
        vOutMinus.mod = vOutMinus.mod + sInPlus - sOutMinus
        v.defaultAncestor = Some(v)

      }


      def ancestor(w: Node3, v: Node3, d: Node3): Node3 = {


        if (inTheBox(w.ancestor).father == v.father) {
          inTheBox(inTheBox(vInMinus).ancestor)
        } else {
          inTheBox(v.defaultAncestor)
        }
      }

    }

    def moveSubtree(wMinus: Node3, wPlus: Node3, shift: Double): Unit = {

      // Encontrar la posición que ocupa wMinus ennte los hermanos. Ese el number

      v.subTrees = wPlus.number - wMinus.number
      wPlus.change = wPlus.change - shift/v.subTrees
      wPlus.shift = wPlus.shift + shift
      wMinus.change = wMinus.change + shift/v.subTrees
      wPlus.prelim = wPlus.prelim + shift
      wPlus.mod = wPlus.mod + shift

    }

  }

  def nextLeft(v: Option[Node3]): Option[Node3] = {
    if (Utils.inTheBox(v).hasChildren) Utils.inTheBox(v).leftMostChild else Utils.inTheBox(v).thread
  }

  def nextRight(v: Option[Node3]): Option[Node3] = {
    if (Utils.inTheBox(v).hasChildren) Utils.inTheBox(v).rightMostChild else Utils.inTheBox(v).thread
  }

  def executeShifts(v: Node3): Unit = {

    v.shift = 0
    v.change = 0

    for (x <- (v.childrenNum - 1) to 0 ) {

      val w = v.children(x)
      w.prelim = w.prelim + v.shift
      w.mod = w.mod + v.shift
      v.change = v.change + w.change
      v.shift = v.shift + w.shift + v.change

    }

  }

}










































