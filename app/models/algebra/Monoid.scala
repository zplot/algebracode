package models.algebra

trait Monoid extends Semigroup {

  type T1
  type T2 <: MonoidElement

  def builder(x: T1): T2

  val structureId: String
  val identity: T2

  override def toString: String = structureId

  trait MonoidElement extends SemigroupElement {

    val fatherMonoid = Monoid.this

  }
}

