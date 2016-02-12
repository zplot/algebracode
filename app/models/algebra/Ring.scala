package models.algebra

trait Ring extends AbelianGroup with Semigroup {



  type T1
  type T2 <: RingElement

  def builder(x: T1): T2

  val structureId: String
  val finite: Boolean
  val one: T2
  val zero: T2

  trait RingElement extends AbelianGroupElement with SemigroupElement {

    val fatherRing = Ring.this


  }
}
