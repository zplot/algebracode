package models.algebra

trait FiniteGroup extends Group {

  type T1
  type T2 <: FiniteGroupElement

  val structureId: String


  def builder(x: T1): T2

  val elements: Set[T2]

  def elementsOrdered: List[T2] = one :: (elements - one).toList

  def cardinal: Int = elements.size

  def isAbelian: Boolean = {
    val producto =
      for (i <- elements; j <- elements) yield i.multiply(j) == j.multiply(i)
    !producto.contains(false)
  }

  def cayleyTable(): Unit = {
    val list1 = (1 to cardinal).toList // Enumeración de los elementos
    val traductor = (list1 zip elementsOrdered).toMap
    val traductorInverso = (list1 zip elementsOrdered).toMap.map(_.swap)
    println()
    println("Cayley table of " + this.structureId + ":")
    println()
    for (i <- 1 to cardinal) {
      for (j <- 1 to cardinal) {
        print(traductorInverso(traductor(i).multiply(traductor(j))) + " ")
      }
      println()
    }
    println()
  }

  def cayleyTable2: String = { // TODO
    val list1 = (1 to cardinal).toList // Enumeración de los elementos
    val traductor = (list1 zip elementsOrdered).toMap
    val traductorInverso = (list1 zip elementsOrdered).toMap.map(_.swap)
    val result = List[String]("w")
    val texto1 = "Cayley table of " + this.structureId + ":"
    val texto2 = List(texto1)
    val texto3 = result ::: texto2
    val texto4 = texto3 ::: List[String]("w")
    val texto5 = texto4 ::: List[String]("w")
    val texto6 =
    for {
        i <- 1 to cardinal
        j <- 1 to cardinal
    } yield traductorInverso(traductor(i).multiply(traductor(j))).toString


    texto6.toString()
  }


  trait FiniteGroupElement extends GroupElement {

    val fatherFiniteGroup = FiniteGroup.this


  }
}
