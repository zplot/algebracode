package models.algebra.examples

import scala.language.implicitConversions
import models.algebra.AbelianGroup

object AbelianGroupExample extends App {

  case object Z extends AbelianGroup {

    type T1 = Int
    type T2 = Entero
    def builder(x: T1): T2 = Entero(x)
    val structureId: String = "Z"
    val zero = builder(0)

    object Entero {

      def apply(k: T1): Entero = {
        new Entero(k)
      }
    }

    class Entero private(val k: T1) extends AbelianGroupElement {

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

  println("Empezamos")

  import Z._

  val cinco = Z.Entero(5)
  val siete = Z.Entero(7)
  val suma = cinco + siete
  val ocho = Z.builder(8)
  val quince = siete + ocho
  println(quince)
  println(suma)
  println(Entero(239))
  println(builder(23))
  println(quince - cinco)
}


