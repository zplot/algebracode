package models.algebra

trait Magma {

  type T1
  type T2 <: MagmaElement

  def builder(x: T1): T2

  val structureId: String
  val one: T2

  override def toString: String = structureId

  trait MagmaElement {

    val fatherMagma = Magma.this
    val elementId: String
    def multiply(other: T2): T2
    def *(other: T2): T2 = this.multiply(other)
    override def toString: String = elementId.toString
  }
}


