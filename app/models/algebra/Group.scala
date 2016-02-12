package models.algebra

trait Group extends Monoid {

  type T1
  type T2 <: GroupElement

  def builder(x: T1): T2

  val structureId: String
  // val one: T2 = identity

  override def toString: String = structureId

  def commutator(g: T2, h: T2): T2 = {
    g.multiply(h.multiply(g.inverse.multiply(h.inverse)))
  }

  def generatedSet(generators: Set[T2]): Set[T2] = {
    def loop(listaAnterior: Set[T2], generators: Set[T2]): Set[T2] = {
      val listaNueva =
        for {
          g <- listaAnterior
          s <- generators
        } yield g.multiply(s)
      if (listaAnterior != listaNueva) {
        loop(listaNueva ++ listaAnterior, generators)
      } else {
        listaAnterior
      }
    }
    val listaInicial = generators + one
    val resultList = loop(listaInicial, generators)
    resultList
  }

  trait GroupElement extends MonoidElement {

    val elementId: String
    val fatherGroup = Group.this

    def inverse: T2
    override def toString: String = elementId.toString
  }
}



