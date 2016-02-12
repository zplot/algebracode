package models.algebra

object Cycle {


  private def normalForm(cic: Cycle): Cycle = cic.ciclo.head match {
    case x if x == cic.ciclo.min => cic
    case _ => normalForm(shiftRight(cic))
  }

  private def shiftRight(c: Cycle): Cycle = {
    Cycle(c.ciclo.last :: c.ciclo.dropRight(1))
  }

  private def next(c: Cycle, z: Int): Int = c.toMap(z)

  private def fromListToMap(list: List[Int]): Map[Int, Int] = {
    list match {
      case Nil => Map[Int, Int]()
      case x :: Nil => Map[Int, Int]()
      case x :: xs => Map(x -> xs.head) ++ fromListToMap(xs)
    }
  }
}

/** A cycle.
  *
  * @param ciclo the cycle as a List[Int]
  *
  * normal: cicle in normal form
  * toList: List[Int] of the cycle in normal form
  * toMap: cycle as a Map[Int, Int]
  */
case class Cycle(ciclo: List[Int]) {

  val normal: Cycle = Cycle.normalForm(this)
  val toList: List[Int] = normal.ciclo
  val length: Int = ciclo.length
  val first: Int = ciclo.head
  val toMap: Map[Int, Int] = Cycle.fromListToMap(ciclo :+ ciclo.head)


  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Cycle]
    if (that == null) false
    else normal.ciclo == that.normal.ciclo
  }

  override def toString: String = "(" + normal.ciclo.mkString(" ") + ")"

}