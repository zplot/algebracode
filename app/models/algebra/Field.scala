package models.algebra

trait Field extends UFD {



  type T1
  type T2 <: FieldElement

  def builder(x: T1): T2

  val structureId: String

  def gcd(a: T2, b: T2): T2 =
    if (a == zero && b == zero) zero else {
      if (b == zero) a else if (a == zero) b else one
    }

  trait FieldElement extends UFDElement {

    val fatherField = Field.this

    def inverse: T2
    def divide(other: T2): T2 = {
      if (other == zero) {
        println()
        println("you are dividing by zero")
        println("this = " + this)
        println("other = " + other)
        println()
        throw new IllegalArgumentException("you are dividing by zero")
      } else {
        this.multiply(other.inverse)
      }
    }
  }
}

