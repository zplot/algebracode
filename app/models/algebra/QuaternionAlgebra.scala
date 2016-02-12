package models.algebra

object QuaternionAlgebra {

}


case class QuaternionAlgebra(ring: Ring) {

  type T0 = ring.T2

  case class QAab (a: T0, b: T0) extends Ring {

    type T1 = (T0,T0,T0,T0)
    type T2 = Quaternion

    def builder(q: T1): Quaternion = Quaternion(q)

    val structureId = "QuaternionAlgebra" + "-" + ring.structureId + "(" + a.toString + "," + b.toString + ")"
    val finite = ring.finite
    val zero = builder(ring.zero,ring.zero,ring.zero,ring.zero)
    override val one = builder(ring.one,ring.zero,ring.zero,ring.zero)
    val identity = ???

    def conjugate(x: T2): T2 = {
      val comp1 = x.q._1
      val comp2 = x.q._2.negate
      val comp3 = x.q._3.negate
      val comp4 = x.q._4.negate
      builder(comp1, comp2, comp3, comp4)
    }
    def norm(x: T2): T0 = {
      val xByXconj = x * conjugate(x)
      xByXconj.q._1
    }

    object Quaternion {
      def apply(q: T1): Quaternion = {
        new Quaternion(q)
      }
    }

    class Quaternion private(val q: T1) extends RingElement {

      val elementId = q.toString()
      val fatherQuaternionAlgebra = QuaternionAlgebra.this
      val isZero = ???

      def add(other: Quaternion): Quaternion = {
        val comp1 = this.q._1 + other.q._1
        val comp2 = this.q._2 + other.q._2
        val comp3 = this.q._3 + other.q._3
        val comp4 = this.q._4 + other.q._4
        builder(comp1, comp2, comp3, comp4)
      }

      def negate: Quaternion = {
        val comp1 = this.q._1.negate
        val comp2 = this.q._2.negate
        val comp3 = this.q._3.negate
        val comp4 = this.q._4.negate
        builder(comp1, comp2, comp3, comp4)
      }

      def minus(other: Quaternion) = {
        val comp1 = this.q._1 + other.q._1.negate
        val comp2 = this.q._2 + other.q._2.negate
        val comp3 = this.q._3 + other.q._3.negate
        val comp4 = this.q._4 + other.q._4.negate
        builder(comp1, comp2, comp3, comp4)
      }
      def multiply(other: Quaternion) = {
        val comp1 = this.q._1 * other.q._1 + this.q._2 * other.q._2 * a + this.q._3 * other.q._3 * b - this.q._4 * other.q._4 * a * b
        val comp2 = this.q._1 * other.q._2 + this.q._2 * other.q._1 - this.q._3 * other.q._4 * b + this.q._4 * other.q._3 * b
        val comp3 = this.q._1 * other.q._3 + this.q._2 * other.q._4 * a + this.q._3 * other.q._1 - this.q._4 * other.q._2 * a
        val comp4 = this.q._1 * other.q._4 + this.q._2 * other.q._3 - this.q._3 * other.q._2 + this.q._4 * other.q._1
        builder(comp1, comp2, comp3, comp4)
      }

      def inverse = ???

      override def toString = q.toString()

      final override def equals(other: Any): Boolean = {
        val that = other.asInstanceOf[Quaternion]
        if (that == null) false
        else {
          q._1 == that.q._1 && this.q._2 == that.q._2 && this.q._3 == that.q._3 && this.q._4 == that.q._4
        }
      }
    }
  }
}




