package models.algebra

object PolynomialOverRing {

  def apply(ring: Ring): PolynomialOverRing = {
    new PolynomialOverRing(ring)
  }
}

class PolynomialOverRing private(val ring: Ring) extends Ring {

  type T1 = Map[Int, ring.T2]
  type T2 = Polynomial


  def builder(x: T1) = Polynomial(x)

  val structureId: String = "Polynomials Over " + ring.structureId

  val finite: Boolean = false

  val zeroPolynomial = builder(Map(0 -> ring.zero))
  val zero = zeroPolynomial

  val x = builder(Map(1 -> ring.one))

  val one = builder(Map(0 -> ring.one))

  object Polynomial {

    def apply(map: T1): T2 = {

      val normalMap = {
        val theMapList = map.toList
        def newMapList(oldMapList: List[(Int, ring.T2)]): List[(Int, ring.T2)] = oldMapList match {
          case Nil => Nil
          case ((x1, ring.zero) :: xs) => newMapList(xs)
          case ((x1, x2) :: xs)  => (x1, x2) :: newMapList(xs)
        }
        newMapList(theMapList).toMap
      }

      new Polynomial(normalMap)
    }

    // Creo que esto es para comparar monomios de acuerdo con el grado
    def comp(monomial1: (Int, ring.T2), monomial2: (Int, ring.T2)) = monomial1._1 > monomial2._1

  }

  class Polynomial private(val map: T1) extends RingElement{

    val fatherPolynomialOverRing = PolynomialOverRing.this
    val elementId = toString
    val isZero = {
      val map = this.map
      val mapKeySet = map.keySet
      val mapValueSet = map.values.toSet
      val c0: Boolean = this == zero
      val c1: Boolean = mapKeySet == Set(0)
      val c2: Boolean = mapValueSet == Set(ring.zero)
      c0 || (c1 && c2)
    }

    def negate = this.multiply(ring.one.negate)

    def add(other: Polynomial) = {

      val tmp3 = ring.zero
      val exponents = (map.keySet ++ other.map.keySet).toList
      def recursion(exp: List[Int]): Map[Int, ring.T2] = exp match {
        case Nil => Map[Int, ring.T2]()
        case x :: xs => recursion(xs) + (x -> (map.getOrElse(x, tmp3) + other.map.getOrElse(x, tmp3)))
      }
      val temporalMap = recursion(exponents)
      val temporalPoly = Polynomial(temporalMap)
      val result = temporalPoly
      result
    }

    def minus(other: T2) = {
      val tmp1 = other
      val tmp2 = ring.one.negate
      val tmp3 = tmp1 * tmp2
      val tmp4 = this.add(tmp3)
      tmp4
    }

    def multiply(other: T2) = { // TODO
      val step1 = for (i <- this.map.toList; j <- other.map.toList) yield (i._1 + j._1, i._2 * j._2)
      val exponents = step1.map(x => x._1).distinct
      val step2 = for (i <- exponents) yield step1.filter(x => x._1 == i)

      def sumListInRing(list1: List[ring.T2]): ring.T2 = list1 match {
        case Nil => ring.zero
        case x :: xs => x + sumListInRing(xs)
      }

      def sumCoef(l: List[(Int, ring.T2)]): (Int, ring.T2) = {
        (l.head._1, sumListInRing(l.map(x => x._2)))
      }

      val step3 = step2.map(sumCoef)
      val step4 = step3.toMap
      Polynomial(step4)

    }

    val degree: Int = {
      //val step1 =  map.keySet
      val step1 = map.keySet
      if (this.map == Map(0 -> ring.zero) || this.map == Map[Int, ring.T2]()) -999999 else step1.max
    }

    val lc: ring.T2 = {
      if (degree == -999999) ring.zero else this.map(degree)
    }

    def multiply(other: ring.T2): T2 = this.multiply(builder(Map(0 -> other)))


    def *(other: ring.T2) = this.multiply(other)

    def isMonic: Boolean = this.lc == ring.one

    override def toString = {
      def printPol(a: List[(Int, ring.T2)]): String = a match {
        case Nil => ""
        case x :: xs if x._1 == 0 => x._2 + " + " + printPol(xs)
        case x :: xs if x._1 == 1 && x._2 == ring.one => "x" + " + " + printPol(xs)
        case x :: xs if x._1 == 1 => x._2 + "x" + " + " + printPol(xs)
        case x :: xs if x._2 == ring.one => "x" + x._1 + " + " + printPol(xs)
        case x :: xs => x._2 + "x" + x._1 + " + " + printPol(xs)

        //"hola hola 189"
      }
      if (this == zeroPolynomial) "0" else printPol(this.map.toList.sortWith(Polynomial.comp)).dropRight(2)
      //map.toString()
    }

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[T2]
      if (that == null) false
      else {
        val a1: Boolean = this.map == that.map
        val a2: Boolean = (this.map == Map[Int, ring.T2]()) && (that.map == Map(0 -> ring.zero))
        val a3: Boolean = (that.map == Map[Int, ring.T2]()) && (this.map == Map(0 -> ring.zero))
        a1 || a2 || a3
      }
    }



  }
}

