package models.botany

import scala.language.implicitConversions


object MTree {
  implicit def string2MTree(s: String): MTree[Char] = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    MTree(s(0), splitChildStrings(1).map(string2MTree(_)))
  }

  def apply[T](x: T) : MTree[T] = MTree[T](x, List())
}

case class MTree[+T](value: T, children: List[MTree[T]]) {

  override def toString = value.toString + children.map(_.toString + "^").mkString("")

}

