package models.algebra

object PermutationGroup {

  def apply(generators: Set[Permutation], name: String) = new PermutationGroup(generators: Set[Permutation], name: String)
  def apply(generators: Set[Permutation]) = new PermutationGroup(generators: Set[Permutation], name = "")

}

class PermutationGroup private(generators: Set[Permutation], name: String) extends FiniteGroup {

  type T1 = Permutation
  type T2 = PermutationGroupElement

  def builder(x: T1): T2 = PermutationGroupElement(x)
  val structureId = "{" + "Permutation Group " + name + "}"

  val identity = builder(Permutation.one)

  val groupId = if (name != "") name else "<" + generators.toString + ">"
  val one: T2 = identity
  val permutationSet = Permutation.generar(generators)
  val elements: Set[T2] = permutationSet.map(x => builder(x))

  // TODO Implementar override def elementsOrdered
  case class PermutationGroupElement(permut: T1) extends FiniteGroupElement {

    val fatherPermutationGroup = PermutationGroup.this
    val elementId = permut.toString

    override def inverse: T2 = builder(this.permut.inver)

    def multiply(other: T2) = builder(this.permut.multiply(other.permut))
  }
}
