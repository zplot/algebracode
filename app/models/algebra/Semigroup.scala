package models.algebra

trait Semigroup extends Magma {

  type T1
  type T2 <: SemigroupElement

  def builder(x: T1): T2

  val structureId: String

  override def toString: String = structureId

  trait SemigroupElement extends MagmaElement {

    val fatherSemigroup = Semigroup.this

  }
}
