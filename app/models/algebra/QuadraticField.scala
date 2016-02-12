package models.algebra

case class QuadraticField(m: Int) extends Ring {

  type T1 = (Int, Int)
  type T2 = QuadraticFieldElement



  def builder(x: T1) = QuadraticFieldElement(x)

  val structureId: String = "Zm(" + m + ")"
  val zero = builder(0, 0)
  override val one = builder(1, 0)  // TODO
  val finite = true
  val identity = ???


  object QuadraticFieldElement {
    def apply(k: T1): QuadraticFieldElement = {
      new QuadraticFieldElement(k)
    }
  }

  class QuadraticFieldElement(val k: T1) extends RingElement {

    val elementId = k.toString()
    val fatherQuadraticField = QuadraticField.this
    val isZero = k == (0,0)

    def add(other: QuadraticFieldElement) = QuadraticFieldElement(k._1 + other.k._1, k._2 + other.k._2)
    def minus(other: QuadraticFieldElement) = QuadraticFieldElement(-k._1, -k._2)
    def multiply(other: QuadraticFieldElement) = QuadraticFieldElement(k._1 * other.k._1 + m * k._2 * other.k._2, k._1 * other.k._2 + k._2 * other.k._1)
    def negate = zero - this
    def inverse = ??? // TODO

    override def toString = k.toString()

    final override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[QuadraticFieldElement]
      if (that == null) false
      else this.k == that.k
    }
  }
}

