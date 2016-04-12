package models.botany

import scala.language.implicitConversions
import scala.language.postfixOps


// Common objects to all Trees

object DrawSettings3 {



// Para nodos numerados
  val factorX: Double = 3
  val factorY: Double = 6
  val shiftX: Double = 40
  val shiftY: Double = 40

  // For circles
  val r = 10
  val stroke = "red"
  val strokeWidth = "1"
  val fill = "yellow"

  // For lines
  val lineStyle = "stroke:rgb(90,90,90);stroke-width:1"

  // For text of nodes
  def shiftTextIdX(p: Point): Double =

    if ((p.id < 20) && (p.id.toInt > 9)) {
      -1 - 3 * math.log10(p.id) - 1
    } else {
      -1 - 3 * math.log10(p.id)
    }

  def shiftTextIdY(p: Point): Double = 3

}

case class Point(id: Int, x: Double, y: Double) {
  override def toString = "(" + x ++ "," + y + ")"
}

case class Edge(pos1: Point, pos2: Point)

case class Draw(points: List[Point], edges: List[Edge])

case class PrintableDraw(points: List[Point], edges: List[Edge])



// Tree3: Numbered nodes

object Tree3Layaut {

  val distance = 14
  val yStep = 10
  var defaultAncestor:Node3 = Utils3.nothing

  var vInPlus: Option[Node3] = None
  var vOutPlus: Option[Node3] = None
  var vInMinus: Option[Node3] = None
  var vOutMinus: Option[Node3] = None
  var sOutPlus: Double = 0
  var sInPlus: Double = 0
  var sInMinus: Double = 0
  var sOutMinus: Double = 0
  var shiftVar: Double = 0
  var changeVar: Double = 0

  def layaut(t: Tree3): Unit = {

    initWalk(t)
    firstWalk(t.root)
    secondWalk(t.root, 0)  // TODO cuál es el segundo argumento?

  }

  def initWalk(tree: Tree3): Unit = {

    import Utils3.inTheBoxFather
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
        import Utils3.inTheBoxFather
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
    import Utils3.inTheBox
    if (v.isLeaf) {
      v.prelim = 0
      if (v.leftSibling isDefined) {
        v.prelim = v.leftSibling.prelim + distance
      }
    } else {
      defaultAncestor = Some(v.children(0))
      for (w <- v.children) {
        firstWalk(w)
        apportion(w, defaultAncestor)
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

  def apportion(v: Node3, defAncest: Node3): Unit = {

    import Utils3.inTheBox

    val w: Node3 = v.leftSibling

    if (w != Utils3.nothing) {   // v tiene leftSibling w

      vInPlus = Some(v)
      vOutPlus = Some(v)
      vInMinus = Some(w)
      vOutMinus = vInPlus.leftMostSibling
      sInPlus = vInPlus.mod
      sOutPlus = vOutPlus.mod
      sInMinus = vInMinus.mod
      sOutMinus = vOutMinus.mod

      while (nextRight(vInMinus).isDefined && nextLeft(vInPlus).isDefined) {

        vInMinus = nextRight(vInMinus)
        vInPlus = nextLeft(vInPlus)
        vOutMinus = nextLeft(vOutMinus)
        vOutPlus = nextRight(vOutPlus)
        vOutPlus.ancestor = Some(v)
        // shiftVar =  -((vInMinus.prelim + sInMinus) - (vInPlus.prelim + sInPlus) + distance)
        // shiftVar =  math.abs((vInMinus.prelim + sInMinus) - (vInPlus.prelim + sInPlus) + distance)
        shiftVar =  (vInMinus.prelim + sInMinus) - (vInPlus.prelim + sInPlus) + distance


        if (shiftVar > 0) {

          moveSubtree(ancestor(inTheBox(vInMinus), v, defAncest), v, shiftVar)
          sInPlus = sInPlus + shiftVar
          sOutPlus = sOutPlus + shiftVar

        }
        sInMinus = sInMinus + vInMinus.mod
        sInPlus = sInPlus + vInPlus.mod
        sOutMinus = sOutMinus + vOutMinus.mod
        sOutPlus = sOutPlus + vOutPlus.mod
      }
    }


    if (nextRight(vInMinus).isDefined && nextRight(vOutPlus).isEmpty) {

      vOutPlus = nextRight(vInMinus)
      vOutPlus.mod = vOutPlus.mod + sInPlus - sOutMinus

    }

    if (nextLeft(vInPlus).isDefined && nextLeft(vOutMinus).isEmpty) {

      vOutMinus.thread = nextLeft(vInPlus)
      vOutMinus.mod = vOutMinus.mod + sInPlus - sOutMinus
      defaultAncestor = Some(v)

    }



    def nextLeft(v: Option[Node3]): Option[Node3] = {
      if (Utils3.inTheBox(v).hasChildren) Utils3.inTheBox(v).leftMostChild else Utils3.inTheBox(v).thread
    }

    def nextRight(v: Option[Node3]): Option[Node3] = {
      if (Utils3.inTheBox(v).hasChildren) Utils3.inTheBox(v).rightMostChild else Utils3.inTheBox(v).thread
    }

    def ancestor(w: Node3, v: Node3, d: Node3): Node3 = {

      if (inTheBox(w.ancestor).father == v.father) {
        inTheBox(w.ancestor)
      } else {
        defaultAncestor
      }
    }

  }

  def moveSubtree(wMinus: Node3, wPlus: Node3, sh: Double): Unit = {

    // Encontrar la posición que ocupa wMinus entre los hermanos. Ese el number

    val subTrees = wPlus.number - wMinus.number
    wPlus.change = wPlus.change - sh/subTrees
    wPlus.shift = wPlus.shift + shiftVar
    wMinus.change = wMinus.change + shiftVar/subTrees
    wPlus.prelim = wPlus.prelim + shiftVar
    wPlus.mod = wPlus.mod + shiftVar

  }

  def executeShifts(v: Node3): Unit = {

    shiftVar = 0
    changeVar = 0

    for (x <- (v.childrenNum - 1) to 0 ) {



      val w = v.children(x)
      w.prelim = w.prelim + shiftVar
      w.mod = w.mod + shiftVar
      changeVar = changeVar + w.change
      shiftVar = shiftVar + w.shift + changeVar

    }

  }

}

object Node3 {

  var lastId = 0

  def apply(children: Vector[Node3]): Node3 = {

    val newId = lastId + 1
    lastId = newId
    new Node3(newId, children)

  }

  def unapply(x: Node3): Option[Vector[Node3]] = Some(x.children)


  // http://aperiodic.net/phil/scala/s-99/
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
    val tmp: Vector[Node3] = splitChildStrings(1).map(string2Tree(_)).toVector
    Node3(tmp)
  }

  implicit def string2Tree3(s: String): Tree3 = {
    val tmp: Node3 = string2Tree(s)
    Tree3(tmp)
  }


  def orderNode3(t: Node3): Node3 = {

    if (t.children != Vector[Node3]())  {
      val tmp3 = t.children.map( x => orderNode3(x))
      val tmp4 = Node3(tmp3)
      val tmp5 = tmp4.children.sortBy(_.weight).reverse
      val tmp6 = Node3(tmp5)
      tmp6
    } else t

  }



}

class Node3(val id: Int, val children: Vector[Node3]) {

  val childrenNum = children.length
  def weight: Int = children.foldLeft(1)(_ + _.weight)
  def canonicalForm: String = Node3.orderNode3(this).toString
  def isLeaf: Boolean = this.children == Vector[Node3]()
  def hasChildren: Boolean = ! isLeaf
  def numChildren = children.length

  var mod: Double = 0
  var thread: Option[Node3] = None
  var ancestor: Option[Node3] = None
  var prelim: Double = 0
  // defaultAncestor es una variable general y no un atributo de un Node3
  // var defaultAncestor: Option[Node3] = None
  var father: Option[Node3] = None // En initWalk
  var leftSibling: Option[Node3] = None // En initWalk
  var leftMostSibling: Option[Node3] = None // En initWalk
  val leftMostChild: Option[Node3] = if (isLeaf) None else Some(children(0))
  val rightMostChild: Option[Node3] = if (isLeaf) None else Some(children(numChildren - 1))
  var shift: Double = 0
  var x: Double = 0
  var y: Double = 0
  var yStep: Double = 10 // Paso de nivel y
  var level: Int = 0 // En initWalk
  var number: Int = -1  // en initWalh
  var subTrees: Int = 0
  var change: Double = 0

  override def toString = "*" + children.map(_.toString + "u").mkString("")

}

object Utils3 {

  val nothing = Node3(Vector[Node3]())

  implicit def inTheBox(z: Option[Node3]): Node3 = z match {
    case None => nothing
    case Some(x) => x
  }

  implicit def inTheBoxFather(z: Option[Node3]): Node3 = z match {
    case None => nothing
    case Some(x) => x
  }

}

object Tree3 {


  def orderTree3(t: Tree3): Tree3 = Node3.string2Tree3(t.canonicalForm)

}

case class Tree3(root: Node3) {

  import DrawSettings3._

  Tree3Layaut.layaut(this)

  val canonicalForm: String = root.canonicalForm

  val nodes: List[Node3] = {
    def loop(s: List[Node3]): List[Node3] = s match {
      case Nil => Nil
      case x :: xs => List(x) ::: loop(x.children.toList) ::: loop(xs)
    }
    root :: loop(this.root.children.toList)
  }

  val nodePoints: List[Point] = {
    nodes.map(node => Point(node.id, node.x, node.y))
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

    val result: List[Edge] = pairs2.map(x => Edge(Point(x._1.id, x._1.x, x._1.y), Point(x._2.id, x._2.x, x._2.y)))
    result

  }

  val drawWidth = {
    val minX: Double = nodePoints.map(p => p.x).min
    val maxX: Double = nodePoints.map(p => p.x).max
    maxX - minX
  }

  val drawHeight = {
    val minY: Double = nodePoints.map(p => p.y).min
    val maxY: Double = nodePoints.map(p => p.x).max
    maxY - minY
  }

  val factorX: Double = if (drawWidth > 1000) 1000 / drawWidth else 3
  val factorY: Double = if (drawHeight > 1000) 1000 / drawWidth else 6

  // Generates a PrintableDraw
  val toPrint = {
    val newPoints = nodePoints.map(point => Point(point.id, point.x * factorX + shiftX, point.y * factorY + shiftY))
    val newEdges = edges.map(edge => {

      val p1X = edge.pos1.x
      val p1Y = edge.pos1.y
      val p2X = edge.pos2.x
      val p2Y = edge.pos2.y

      // Estas son las coordenadas de los nodos antes de corregir el que las líneas enren en los círculos
      val newp1X = p1X * factorX + shiftX
      val newp1Y = p1Y * factorY + shiftY
      val newp2X = p2X * factorX + shiftX
      val newp2Y = p2Y * factorY + shiftY


      // Removing lines inside circles
      val slope: Float = if (math.abs(newp2X - newp1X) > 1) {
        // The edge is not vertical
        (newp2Y - newp1Y).toFloat / (newp2X - newp1X).toFloat
      } else {
        99999 // The edge is vertical
      }

      val sqrtOfOnePlusTg2betha = math.sqrt(1 + slope * slope).toFloat

      val deltaX: Double = r.toDouble / sqrtOfOnePlusTg2betha
      val deltaY: Double = r.toDouble * math.sqrt(1 - deltaX * deltaX / r.toFloat / r.toFloat).toFloat



      val defp1X: Double = if (slope < 0) {
        newp1X + deltaX
      } else {
        newp1X - deltaX
      }

      val defp1Y: Double = if (slope < 0) {
        newp1Y - deltaY
      } else {
        newp1Y - deltaY
      }

      val defp2X: Double = if (slope < 0) {
        newp2X - deltaX
      } else {
        newp2X + deltaX
      }

      val defp2Y: Double = if (slope < 0) {
        newp2Y + deltaY
      } else {
        newp2Y + deltaY
      }



      val defp1XInt = defp1X.toInt
      val defp1YInt = defp1Y.toInt
      val defp2XInt = defp2X.toInt
      val defp2YInt = defp2Y.toInt

      // Con corrección de invasión de círculos
      Edge(Point(edge.pos1.id, defp1XInt, defp1YInt), Point(edge.pos2.id, defp2XInt, defp2YInt))

      // Sin corrección de invasión de círculos
      //Edge(Point(newp1X, newp1Y), Point(newp2X, newp2Y))
    }
    )
    PrintableDraw(newPoints, newEdges)
  }


  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Tree3]
    if (that == null) false
    else this.root.canonicalForm == that.root.canonicalForm
  }

}









// Tree4: identical nodes

object Tree4Layaut {

  val distance = 14
  val yStep = 10
  var defaultAncestor:Node4 = Utils4.nothing

  var vInPlus: Option[Node4] = None
  var vOutPlus: Option[Node4] = None
  var vInMinus: Option[Node4] = None
  var vOutMinus: Option[Node4] = None
  var sOutPlus: Double = 0
  var sInPlus: Double = 0
  var sInMinus: Double = 0
  var sOutMinus: Double = 0
  var shiftVar: Double = 0
  var changeVar: Double = 0

  def layaut(t: Tree4): Unit = {

    initWalk(t)
    firstWalk(t.root)
    secondWalk(t.root, 0)  // TODO cuál es el segundo argumento?

  }

  def initWalk(tree: Tree4): Unit = {

    import Utils4.inTheBoxFather
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

    def initNextLevel(n: Node4): Unit = {

      for (t <- n.children) {
        t.mod = 0
        t.thread = None
        t.ancestor = Some(t)
        t.father = Some(n)
        t.level = n.level + 1
        t.number = {
          val siblings: Vector[Node4] = t.father.children
          val mapa: Map[Node4, Int] = if (siblings == Vector()) Map() else siblings.zipWithIndex.toMap
          if (mapa == Map()) -100 else mapa(t)
        }
        import Utils4.inTheBoxFather
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

  def firstWalk(v: Node4): Unit = {
    import Utils4.inTheBox
    if (v.isLeaf) {
      v.prelim = 0
      if (v.leftSibling isDefined) {
        v.prelim = v.leftSibling.prelim + distance
      }
    } else {
      defaultAncestor = Some(v.children(0))
      for (w <- v.children) {
        firstWalk(w)
        apportion(w, defaultAncestor)
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

  def secondWalk(v: Node4, m: Double): Unit = {

    v.x = v.prelim + m
    v.y = v.level * yStep
    for (w <- v.children) {
      secondWalk(w, m + v.mod)
    }
  }

  def apportion(v: Node4, defAncest: Node4): Unit = {

    import Utils4.inTheBox

    val w: Node4 = v.leftSibling

    if (w != Utils4.nothing) {   // v tiene leftSibling w

      vInPlus = Some(v)
      vOutPlus = Some(v)
      vInMinus = Some(w)
      vOutMinus = vInPlus.leftMostSibling
      sInPlus = vInPlus.mod
      sOutPlus = vOutPlus.mod
      sInMinus = vInMinus.mod
      sOutMinus = vOutMinus.mod

      while (nextRight(vInMinus).isDefined && nextLeft(vInPlus).isDefined) {

        vInMinus = nextRight(vInMinus)
        vInPlus = nextLeft(vInPlus)
        vOutMinus = nextLeft(vOutMinus)
        vOutPlus = nextRight(vOutPlus)
        vOutPlus.ancestor = Some(v)
        // shiftVar =  -((vInMinus.prelim + sInMinus) - (vInPlus.prelim + sInPlus) + distance)
        // shiftVar =  math.abs((vInMinus.prelim + sInMinus) - (vInPlus.prelim + sInPlus) + distance)
        shiftVar =  (vInMinus.prelim + sInMinus) - (vInPlus.prelim + sInPlus) + distance


        if (shiftVar > 0) {

          moveSubtree(ancestor(inTheBox(vInMinus), v, defAncest), v, shiftVar)
          sInPlus = sInPlus + shiftVar
          sOutPlus = sOutPlus + shiftVar

        }
        sInMinus = sInMinus + vInMinus.mod
        sInPlus = sInPlus + vInPlus.mod
        sOutMinus = sOutMinus + vOutMinus.mod
        sOutPlus = sOutPlus + vOutPlus.mod
      }
    }


    if (nextRight(vInMinus).isDefined && nextRight(vOutPlus).isEmpty) {

      vOutPlus = nextRight(vInMinus)
      vOutPlus.mod = vOutPlus.mod + sInPlus - sOutMinus

    }

    if (nextLeft(vInPlus).isDefined && nextLeft(vOutMinus).isEmpty) {

      vOutMinus.thread = nextLeft(vInPlus)
      vOutMinus.mod = vOutMinus.mod + sInPlus - sOutMinus
      defaultAncestor = Some(v)

    }



    def nextLeft(v: Option[Node4]): Option[Node4] = {
      if (Utils4.inTheBox(v).hasChildren) Utils4.inTheBox(v).leftMostChild else Utils4.inTheBox(v).thread
    }

    def nextRight(v: Option[Node4]): Option[Node4] = {
      if (Utils4.inTheBox(v).hasChildren) Utils4.inTheBox(v).rightMostChild else Utils4.inTheBox(v).thread
    }

    def ancestor(w: Node4, v: Node4, d: Node4): Node4 = {

      if (inTheBox(w.ancestor).father == v.father) {
        inTheBox(w.ancestor)
      } else {
        defaultAncestor
      }
    }

  }

  def moveSubtree(wMinus: Node4, wPlus: Node4, sh: Double): Unit = {

    // Encontrar la posición que ocupa wMinus entre los hermanos. Ese el number

    val subTrees = wPlus.number - wMinus.number
    wPlus.change = wPlus.change - sh/subTrees
    wPlus.shift = wPlus.shift + shiftVar
    wMinus.change = wMinus.change + shiftVar/subTrees
    wPlus.prelim = wPlus.prelim + shiftVar
    wPlus.mod = wPlus.mod + shiftVar

  }

  def executeShifts(v: Node4): Unit = {

    shiftVar = 0
    changeVar = 0

    for (x <- (v.childrenNum - 1) to 0 ) {



      val w = v.children(x)
      w.prelim = w.prelim + shiftVar
      w.mod = w.mod + shiftVar
      changeVar = changeVar + w.change
      shiftVar = shiftVar + w.shift + changeVar

    }

  }

}

object Node4 {

  var lastId = 0

  def apply(children: Vector[Node4]): Node4 = {

    val newId = lastId + 1
    lastId = newId
    new Node4(newId, children)

  }

  def unapply(x: Node4): Option[Vector[Node4]] = Some(x.children)


  // http://aperiodic.net/phil/scala/s-99/
  implicit def string2Tree(s: String): Node4 = {

    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == 'u') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    val tmp: Vector[Node4] = splitChildStrings(1).map(string2Tree(_)).toVector
    Node4(tmp)
  }

  implicit def string2Tree4(s: String): Tree4 = {
    val tmp: Node4 = string2Tree(s)
    Tree4(tmp)
  }


  def orderNode4(t: Node4): Node4 = {

    if (t.children != Vector[Node4]())  {
      val tmp3 = t.children.map( x => orderNode4(x))
      val tmp4 = Node4(tmp3)
      val tmp5 = tmp4.children.sortBy(_.weight).reverse
      val tmp6 = Node4(tmp5)
      tmp6
    } else t

  }



}

class Node4(val id: Int, val children: Vector[Node4]) {

  val childrenNum = children.length
  def weight: Int = children.foldLeft(1)(_ + _.weight)
  def canonicalForm: String = Node4.orderNode4(this).toString
  def isLeaf: Boolean = this.children == Vector[Node4]()
  def hasChildren: Boolean = ! isLeaf
  def numChildren = children.length

  var mod: Double = 0
  var thread: Option[Node4] = None
  var ancestor: Option[Node4] = None
  var prelim: Double = 0
  // defaultAncestor es una variable general y no un atributo de un Node4
  // var defaultAncestor: Option[Node4] = None
  var father: Option[Node4] = None // En initWalk
  var leftSibling: Option[Node4] = None // En initWalk
  var leftMostSibling: Option[Node4] = None // En initWalk
  val leftMostChild: Option[Node4] = if (isLeaf) None else Some(children(0))
  val rightMostChild: Option[Node4] = if (isLeaf) None else Some(children(numChildren - 1))
  var shift: Double = 0
  var x: Double = 0
  var y: Double = 0
  var yStep: Double = 10 // Paso de nivel y
  var level: Int = 0 // En initWalk
  var number: Int = -1  // en initWalh
  var subTree4s: Int = 0
  var change: Double = 0

  override def toString = "*" + children.map(_.toString + "u").mkString("")

}

object Utils4 {

  val nothing = Node4(Vector[Node4]())

  implicit def inTheBox(z: Option[Node4]): Node4 = z match {
    case None => nothing
    case Some(x) => x
  }

  implicit def inTheBoxFather(z: Option[Node4]): Node4 = z match {
    case None => nothing
    case Some(x) => x
  }

}

object Tree4 {

  // Following A Very Basic Introduction to Hopf Algebras by J.M. Selig
  def bMinus(t: Tree4): Forest4 = {
    val tmp = for { x <- t.root.children } yield Tree4(x)
    Forest4(tmp)
  }
  // Following A Very Basic Introduction to Hopf Algebras by J.M. Selig
  def bPlus(f:Forest4): Tree4 = Tree4(Node4(f.Tree4s.map( x => x.root)))

  def orderTree4(t: Tree4): Tree4 = Node4.string2Tree4(t.canonicalForm)

}

case class Tree4(root: Node4) {

  import DrawSettings3._

  Tree4Layaut.layaut(this)

  val canonicalForm: String = root.canonicalForm

  val nodes: List[Node4] = {
    def loop(s: List[Node4]): List[Node4] = s match {
      case Nil => Nil
      case x :: xs => List(x) ::: loop(x.children.toList) ::: loop(xs)
    }
    root :: loop(this.root.children.toList)
  }

  val nodePoints: List[Point] = {
    nodes.map(node => Point(node.id, node.x, node.y))
  }

  val edges: List[Edge] = {

    val pairs: List[(Node4, Option[Node4])] = nodes.map(x => (x, x.father))

    val pairs2: List[(Node4, Node4)] = {
      val tmp: List[(Node4, Option[Node4])] = pairs.filterNot(x => x ==(x._1, None))
      def transform(x: (Node4, Option[Node4])): (Node4, Node4) = x match {
        case (a, Some(b)) => (a, b)
        case (a, None) => (a, a)
      }
      val tmp2: List[(Node4, Node4)] = tmp.map(x => transform(x))
      tmp2
    }

    val result: List[Edge] = pairs2.map(x => Edge(Point(x._1.id, x._1.x, x._1.y), Point(x._2.id, x._2.x, x._2.y)))
    result

  }

  val drawWidth = {
    val minX: Double = nodePoints.map(p => p.x).min
    val maxX: Double = nodePoints.map(p => p.x).max
    maxX - minX
  }

  val drawHeight = {
    val minY: Double = nodePoints.map(p => p.y).min
    val maxY: Double = nodePoints.map(p => p.x).max
    maxY - minY
  }

  val factorX: Double = if (drawWidth > 1000) 1000 / drawWidth else 3
  val factorY: Double = if (drawHeight > 1000) 1000 / drawWidth else 6

  // Generates a PrintableDraw
  val toPrint = {
    val newPoints = nodePoints.map(point => Point(point.id, point.x * factorX + shiftX, point.y * factorY + shiftY))
    val newEdges = edges.map(edge => {

      val p1X = edge.pos1.x
      val p1Y = edge.pos1.y
      val p2X = edge.pos2.x
      val p2Y = edge.pos2.y

      // Estas son las coordenadas de los nodos antes de corregir el que las líneas enren en los círculos
      val newp1X = p1X * factorX + shiftX
      val newp1Y = p1Y * factorY + shiftY
      val newp2X = p2X * factorX + shiftX
      val newp2Y = p2Y * factorY + shiftY


      // Removing lines inside circles
      val slope: Float = if (math.abs(newp2X - newp1X) > 1) {
        // The edge is not vertical
        (newp2Y - newp1Y).toFloat / (newp2X - newp1X).toFloat
      } else {
        99999 // The edge is vertical
      }

      val sqrtOfOnePlusTg2betha = math.sqrt(1 + slope * slope).toFloat

      val deltaX: Double = r.toDouble / sqrtOfOnePlusTg2betha
      val deltaY: Double = r.toDouble * math.sqrt(1 - deltaX * deltaX / r.toFloat / r.toFloat).toFloat



      val defp1X: Double = if (slope < 0) {
        newp1X + deltaX
      } else {
        newp1X - deltaX
      }

      val defp1Y: Double = if (slope < 0) {
        newp1Y - deltaY
      } else {
        newp1Y - deltaY
      }

      val defp2X: Double = if (slope < 0) {
        newp2X - deltaX
      } else {
        newp2X + deltaX
      }

      val defp2Y: Double = if (slope < 0) {
        newp2Y + deltaY
      } else {
        newp2Y + deltaY
      }



      val defp1XInt = defp1X.toInt
      val defp1YInt = defp1Y.toInt
      val defp2XInt = defp2X.toInt
      val defp2YInt = defp2Y.toInt

      // Con corrección de invasión de círculos
      Edge(Point(edge.pos1.id, defp1XInt, defp1YInt), Point(edge.pos2.id, defp2XInt, defp2YInt))

      // Sin corrección de invasión de círculos
      //Edge(Point(newp1X, newp1Y), Point(newp2X, newp2Y))
    }
    )
    PrintableDraw(newPoints, newEdges)
  }


  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Tree4]
    if (that == null) false
    else this.root.canonicalForm == that.root.canonicalForm
  }

}

case class Forest4(Tree4s: Vector[Tree4]) {

}












































