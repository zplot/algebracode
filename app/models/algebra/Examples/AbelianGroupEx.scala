package models.algebra.Examples

import models.algebra.AbelianGroup

object AbelianGroupEx extends App {

  case object Z extends AbelianGroup {

    type T1 = Int
    type T2 = Entero

    def builder(x: T1) = Entero(x)
    val structureId: String = "Z"
    val zero = builder(0)

    object Entero {

      def apply(k: T1): Entero = {
        new Entero(k)
      }
    }

    class Entero private(val k: T1) extends AbelianGroupElement {

      val elementId = k.toString
      val isZero = k == 0
      def add(other: Entero) = Entero(k + other.k)
      def minus(other: Entero) = Entero(k - other.k)
      def negate: T2 = Entero(-k)
      override def toString = k.toString
      def minus = Entero(-this.k)
      final override def equals(other: Any): Boolean = {
        val that = other.asInstanceOf[Entero]
        if (that == null) false
        else this.k == that.k
      }
    }

  }

}


