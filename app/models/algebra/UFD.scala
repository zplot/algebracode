package models.algebra

trait UFD extends Ring {

  type T1
  type T2 <: UFDElement

  def builder(x: T1): T2

  val structureId: String

  def gcd(a: T2, b: T2): T2

  trait UFDElement extends RingElement {

    val fatherUFD = UFD.this

  }
}
