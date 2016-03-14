package models.botany

import scala.language.implicitConversions



// Rooted Trees
object RTree {

  implicit def string2RTree(s: String): RTree = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    val tmp = splitChildStrings(1).map(string2RTree(_)).toVector
    RTree(4, tmp)
  }

  def orderRTree(t: RTree): RTree = {

    if (t.children != Vector())  {
      val tmp3 = t.children.map( x => orderRTree(x))
      val tmp4 = RTree(4, tmp3)
      val tmp5 = tmp4.children.sortBy(_.weight).reverse
      val tmp6 = RTree(5, tmp5)
      tmp6
    } else t

  }

}

object Node2 {

  val state = List[Int](0)
  def next(state: List[Int]): (List[Int], Int) = {

    ((state.head + 1) :: state, state.head + 1)

  }



  def apply(id: Int): Node2 ={

    val newId = next(state)._2
    new Node2(newId)
  }
}

class Node2 private(id: Int) {
  var mod: Int = 0
  var thread: Int = 0
  var ancestor: Node2 = this
}


case class RTree(id: Int, children: Vector[RTree]) {

  def isEmpty: Boolean = children == Vector()
  def weight: Int = children.foldLeft(1)(_ + _.weight)

  override def toString = "*" + children.map(_.toString + "^").mkString("")

  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[RTree]
    if (that == null) false
    else RTree.orderRTree(this).children == RTree.orderRTree(that).children
  }

}



