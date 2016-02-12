package models.algebra

case class GroupRing(group: FiniteGroup, ring:Ring ) {

  type T1 = Vector[ring.T2]
  type T2 = GroupRingElement


  val dim = group.cardinal

  val rngZero: this.ring.T2 = ring.zero
  val rngOne: this.ring.T2 = ring.one
  val zero = GroupRingElement(List.fill(dim)(rngZero).toVector)
  // El 0 del GroupRing
  val one = GroupRingElement(List.fill(dim - 1)(rngZero).toVector.+:(rngOne))
  // El 1 del GroupRing

  val rngMinusOne = ring.zero.minus(ring.one)

  val groupRingId: String = group.structureId + ring.structureId
  val finite: Boolean = true

  def builder(x:T1): T2 = GroupRingElement(x)

  // Solo manejamos grupos finitos
  val orderList = group.one :: (group.elements - group.one).toList
  // Orden de los elementos del grupo. El 1 es el 1º
  val ord = orderList.toIndexedSeq

  case class GroupRingElement(vector: Vector[ring.T2]) {

    val fatherGroupRing = GroupRing.this

    def add(other: GroupRingElement) = {
      def recursiveAdd(vector1: Vector[ring.T2], vector2: Vector[ring.T2]): Vector[ring.T2] = {

        val list1 = vector1.toList
        val list2 = vector2.toList
        (list1, list2) match {

          case (Nil, Nil) => Vector[ring.T2]()
          case (x :: xs, y :: ys) => x.add(y) +: recursiveAdd(xs.toVector, ys.toVector)
          case (Nil, y) => y.toVector // Este caso nunca se da. Lo ponemos para que el compilador no cante
          case (x, Nil) => x.toVector // Este caso nunca se da. Lo ponemos para que el compilador no cante
        }
      }
      GroupRingElement(recursiveAdd(this.vector, other.vector))
    }

    def minus(other: T2) = {
      val tmp1 = rngMinusOne
      val tmp2 = other
      this.add(other.multiply(rngMinusOne))
    }

    def multiply(other: GroupRingElement) = {
      // Se come las coordenadas y devuelve la coordenada correspondiente a la multiplicación
      def mult(i: Int, j: Int): (ring.T2, Int) = {
        val elemGroup = ord.indexOf(ord(i).multiply(ord(j)))
        val e1 = this.vector(i)
        val e2 = other.vector(j)
        val elemRing = e1.multiply(e2)
        (elemRing, elemGroup)
      }

      val tmp1 = for (i <- 0 to dim - 1; j <- 0 to dim - 1) yield mult(i, j)
      val tmp2 = tmp1.toList // En cada par, (coeficiente, posición del elemento del grupo)
      val tmp3 = for (i <- 0 to dim - 1) yield tmp2.filter(x => x._2 == i)
      def fun1(lista: List[(ring.T2, Int)]): List[ring.T2] = lista.map(x => x._1)
      val tmp4 = tmp3.map(fun1)
      def sumT(x: ring.T2, y: ring.T2): ring.T2 = x.add(y)
      val tmp5 = tmp4.toList
      val tmp6 = tmp5.map(x => x.fold(rngZero)((a1, a2) => sumT(a1, a2)))
      val tmp7 = tmp6.toVector
      GroupRingElement(tmp7)
    }

    def multiply(other: ring.T2): GroupRingElement = {
      val tmp = GroupRingElement(List.fill(dim - 1)(rngZero).toVector.+:(rngOne.multiply(other)))
      this.multiply(tmp)
    }

    def *(other: T2) = this.multiply(other)
    def *(other: ring.T2) = this.multiply(other)
    def +(other: T2) = this.add(other)
    def -(other: T2) = this.minus(other)

    override def toString = this.vector.toString() + "\n" + ord.toString() + "\n"
  }

}


