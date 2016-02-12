package models.algebra

object Permutation {


  def fromListOfCyclestoMap(lc: List[Cycle]): Map[Int, Int] = {

    lc match {
      case Nil => Map[Int, Int]()
      case x :: xs => x.toMap ++ fromListOfCyclestoMap(xs)

    }
  }

  val one = Permutation(Set(Cycle(List(1))))

  def generar(generadores: Set[Permutation]): Set[Permutation] = {

    def bucleMultiple(listaAnterior: Set[Permutation], generadores: Set[Permutation]): Set[Permutation] = {

      val listaNueva =
        for {
          g <- listaAnterior
          s <- generadores
        } yield g.multiply(s)
      if (listaAnterior != listaNueva) {
        bucleMultiple(listaNueva ++ listaAnterior, generadores)
      } else {
        listaAnterior
      }
    }
    val listaInicial = generadores + one
    bucleMultiple(listaInicial, generadores)
  }
}


case class Permutation(struc: Set[Cycle]) {
  // struc es una estructura de ciclos disjuntos


  // Estructtura de ciclos disjuntos sin ciclos de longitud 1
  val strucOK: List[Cycle] = {

    val tmp1 = struc.toList // Lista de ciclos
    val tmp2 = tmp1.filter(x => x.ciclo.length > 1) // sólo long > 1
    tmp2

  }

  val numCiclos: Int = strucOK.length

  val cicloMasLargo: Int = {

    val tmp1: List[Cycle] = struc.toList
    val tmp2 = tmp1.map(x => x.length)
    tmp2.max

  }

  // Transformamos una permutación en un mapa
  val toMap: Map[Int, Int] = Permutation.fromListOfCyclestoMap(strucOK)

  // lowest canonical order
  val lco: List[Cycle] = {

    def orden(cycle1: Cycle, cycle2: Cycle): Cycle = (cycle1.toList, cycle2.toList) match {
      case (List(), List()) => cycle1
      case (x, List()) => cycle1
      case (List(), y) => cycle2
      case (x, y) if x.length < y.length => cycle1
      case (x, y) if x.length > y.length => cycle2
      case (x :: xs, y :: ys) if x < y => cycle1
      case (x :: xs, y :: ys) if x > y => cycle2
      case _ => cycle1

    }
    strucOK.sortWith((cycle1, cycle2) => orden(cycle1, cycle2) == cycle1)
  }

  override def toString: String = if (lco == List()) "1" else "P" + lco.mkString("")

  def inver: Permutation = {

    // TODO El inverso de 1 ha de ser 1. Ahora casca

    if (cicloMasLargo < 2) Permutation(Set(Cycle(List(1))))
    else {
      // El inverso de 1 es 1
      val tmp1 = this.lco
      val tmp2 = tmp1.map(x => x.ciclo)
      val tmp3 = tmp2.map(x => x.reverse)
      val tmp4 = tmp3.map(x => Cycle(x))
      val tmp5 = tmp4.toSet
      Permutation(tmp5)
    }

  }

  def multiply(other: Permutation): Permutation = (this, other) match {

    case (Permutation.one, Permutation.one) => Permutation.one
    case (x, Permutation.one) => x
    case (Permutation.one, x) => x
    case (_, _) =>

      val izda: List[Cycle] = this.strucOK
      val dcha: List[Cycle] = other.strucOK
      val tmp1: List[Cycle] = izda ++ dcha
      val seMueven: List[Int] = tmp1.flatMap(x => x.ciclo).distinct.sorted
      val mapaProducto: Map[Int, Int] =
        seMueven.foldRight[Map[Int, Int]](Map[Int, Int]())((i, mapa) =>
          mapa ++ Map(i -> this.toMap.getOrElse(other.toMap.getOrElse(i, i), other.toMap.getOrElse(i, i))))
      val tmp2 = mapaProducto.toList

      def concatenar(mapa: Map[Int, Int], lista: List[Int]): List[Int] = {
        if (!mapa.contains(lista.last)) {
          lista.dropRight(1)
        } else {
          concatenar(mapa - lista.last, lista :+ mapa(lista.last))
        }
      }

      def extinguir(mapa: Map[Int, Int], quedan: List[Int], ciclos: List[List[Int]]): List[List[Int]] = {

        if (quedan == List()) {
          ciclos
        } else {
          val cicloSiguiente = concatenar(mapa, List(quedan.head))
          val resultado1 = extinguir(mapa, quedan diff ciclos.flatten, ciclos ++ List(cicloSiguiente)).distinct
          resultado1
        }
      }


      val resultado1 = extinguir(mapaProducto, seMueven, List())
      val resultado2 = resultado1.map(x => Cycle(x)).toSet
      Permutation(resultado2)
  }

  // Redefinimos equals y hashCode
  override def equals(o: Any) = o match {

    case that: Permutation => that.lco == this.lco
    case _ => false

  }

  override def hashCode = lco.hashCode


}


