package models.botany

import scala.language.implicitConversions
import scala.language.postfixOps




case class Point(x: Double, y: Double) {
  override def toString = "(" + x ++ "," + y + ")"
}

case class Edge(pos1: Point, pos2: Point)

case class Draw(points: List[Point], edges: List[Edge])

case class PrintableDraw(points: List[Point], edges: List[Edge])

object DrawSettings {

  val factor = 5
  val shiftX = 40
  val shiftY = 100

  // For circles
  val r ="6"
  val stroke = "black"
  val strokeWidth = "1"
  val fill = "red"

  // For lines
  val lineStyle = "stroke:rgb(40,40,40);stroke-width:1"

}



// http://aperiodic.net/phil/scala/s-99/
object Node3 {

  var lastId = 0

  def apply(children: Vector[Node3]): Node3 = {

    val newId = lastId + 1
    lastId = newId
    new Node3(newId, children)

  }

  def unapply(x: Node3): Option[Vector[Node3]] = Some(x.children)



  implicit def string2Tree(s: String): Node3 = {

    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == 'u') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    val tmp = splitChildStrings(1).map(string2Tree(_)).toVector
    Node3(tmp)
  }

  implicit def string2Tree3(s: String): Tree3 = {
    val tmp = string2Tree(s)
    Tree3(tmp)
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



case class Tree3(root: Node3) {

  import DrawSettings._

  TreeLayaut.layaut(this)

  val nodes: List[Node3] = {
    def loop(s: List[Node3]): List[Node3] = s match {
      case Nil => Nil
      case x :: xs => List(x) ::: loop(x.children.toList) ::: loop(xs)
    }
    root :: loop(this.root.children.toList)
  }

  val nodePoints: List[Point] = {
    nodes.map(node => Point(node.x, node.y))
  }

  val edges: List[Edge] = {

    val pairs: List[(Node3, Option[Node3])] = nodes.map(x => (x, x.father))

    val pairs2: List[(Node3, Node3)] = {
      val tmp: List[(Node3, Option[Node3])] = pairs.filterNot(x => x ==(x._1, None))
      def transform(x: (Node3, Option[Node3])): (Node3, Node3) = x match {
        case (a, Some(b)) => (a, b)
        case (a, None) => (a, a)
      }
      val tmp2: List[(Node3, Node3)] = tmp.map(x => transform(x))
      tmp2
    }

    val result: List[Edge] = pairs2.map(x => Edge(Point(x._1.x, x._1.y), Point(x._2.x, x._2.y)))
    result

  }

  // TODO Hay que revisar esto
  val toPrint = {

    val newPoints = nodePoints.map(point => Point(point.x * factor + shiftX, point.y * factor + shiftY))

    val newEdges = edges.map(edge => {

      val p1X = edge.pos1.x
      val p1Y = edge.pos1.y
      val p2X = edge.pos2.x
      val p2Y = edge.pos2.y

      // Estas son las coordenadas de los nodos antes de corregir el que las líneas enren en los círculos
      val newp1X = p1X * factor + shiftX
      val newp1Y = p1Y * factor + shiftY
      val newp2X = p2X * factor + shiftX
      val newp2Y = p2Y * factor + shiftY


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
      val defp1Y = newp1Y - deltaY
      val defp2X = newp2X - deltaX
      val defp2Y = newp2Y + deltaY

      val defp1XInt = defp1X.toInt
      val defp1YInt = defp1Y.toInt
      val defp2XInt = defp2X.toInt
      val defp2YInt = defp2Y.toInt

      // Con corrección de invasión de círculos
      Edge(Point(defp1XInt, defp1YInt), Point(defp2XInt, defp2YInt))

      // Sin corrección de invasión de círculos
      //Edge(Point(newp1X, newp1Y), Point(newp2X, newp2Y))
    }
    )

    PrintableDraw(newPoints, newEdges)
  }

  override def toString = "Tree3\n" + "Nodes: " + nodes.toString + "\n" + "Edges: " + edges.toString()

}

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
  //override def toString = "*" + children.map(_.toString + "^").mkString("")
  // override def toString = id.toString + "-" + children.toString()

/*  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Node3]
    if (that == null) false
    else Node3.orderTree(this).children == Node3.orderTree(that).children
  }*/

  override def toString = id.toString()

}

object TreeLayaut {

  val distance = 10
  val yStep = 10

  def layaut(t: Tree3): Unit = {

    initWalk(t)
    firstWalk(t.root)
    secondWalk(t.root, 0)  // TODO cuál es el segundo argumento?


  }

  def initWalk(tree: Tree3): Unit = {

    import Utils.inTheBoxFather
    val root = tree.root
    root.father = None
    root.level = 0
    root.leftSibling = None
    root.leftMostSibling = None
    root.number = -1
    root.mod = 0
    root.thread = None
    root.ancestor = Some(root)
    initNextLevel(root)

    def initNextLevel(n: Node3): Unit = {

      for (t <- n.children) {
        t.mod = 0
        t.thread = None
        t.ancestor = Some(t)
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
      if (v.leftSibling isDefined) {
        v.prelim = v.leftSibling.prelim + distance
      }
    } else {
      v.defaultAncestor = Some(v.children(0))
      for (w <- v.children) {
        firstWalk(w)
        apportion(w, w.defaultAncestor)
      }
      executeShifts(v)
      val midpoint = 1.0 / 2.0 * (v.children(0).prelim + v.children(v.childrenNum - 1).prelim)
      v.leftSibling match {
        case None => {
          v.prelim = midpoint
        }
        case Some(w) => {
          v.prelim = w.prelim + distance
          v.mod = v.prelim - midpoint
        }
      }

    }
  }

  def secondWalk(v: Node3, m: Double): Unit = {

    v.x = v.prelim + m
    v.y = v.level * yStep
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

    }

    def nextLeft(v: Option[Node3]): Option[Node3] = {
      if (Utils.inTheBox(v).hasChildren) Utils.inTheBox(v).leftMostChild else Utils.inTheBox(v).thread
    }

    def nextRight(v: Option[Node3]): Option[Node3] = {
      if (Utils.inTheBox(v).hasChildren) Utils.inTheBox(v).rightMostChild else Utils.inTheBox(v).thread
    }

    def ancestor(w: Node3, v: Node3, d: Node3): Node3 = {

      if (inTheBox(w.ancestor).father == v.father) {
        inTheBox(w.ancestor)
      } else {
        inTheBox(v.defaultAncestor)
      }
    }

  }

  def moveSubtree(wMinus: Node3, wPlus: Node3, shift: Double): Unit = {

    // Encontrar la posición que ocupa wMinus ennte los hermanos. Ese el number

    val subTrees = wPlus.number - wMinus.number
    wPlus.change = wPlus.change - shift/subTrees
    wPlus.shift = wPlus.shift + shift
    wMinus.change = wMinus.change + shift/subTrees
    wPlus.prelim = wPlus.prelim + shift
    wPlus.mod = wPlus.mod + shift

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










































