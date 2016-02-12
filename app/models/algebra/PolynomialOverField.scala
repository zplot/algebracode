package models.algebra

/**
 * builder of a Polynomial Ring over field
 */
object PolynomialOverField {

  def apply(field: Fp): PolynomialOverField = {
    new PolynomialOverField(field)
  }
}

/**
 * builder of a Polynomial Ring over field
 */
class PolynomialOverField private(val field: Field)  {

  type T1 = Map[Int, field.T2]
  type T2 = Polynomial

  def builder(x: T1) = Polynomial(x)

  // See Victor Shoup pag. 472
  def gcdExtended(g: Polynomial, h: Polynomial): (Polynomial, Polynomial, Polynomial) = {

    val r: Polynomial = g
    val rPrime: Polynomial = h
    val s: Polynomial = builder(Map(0 -> field.one))
    val sPrime: Polynomial = zeroPolynomial
    val t: Polynomial = zeroPolynomial
    val tPrime: Polynomial = builder(Map(0 -> field.one))

    def loop(r: Polynomial, s: Polynomial, t: Polynomial, rPrime: Polynomial, sPrime: Polynomial,tPrime: Polynomial): (Polynomial, Polynomial, Polynomial,Polynomial, Polynomial, Polynomial) = {

      if (rPrime != zeroPolynomial) {
        val (q, rPrimePrime) = r / rPrime
        loop(rPrime, sPrime, tPrime, rPrimePrime, s - sPrime * q, t - tPrime * q)
      } else {
        val c = r.lc
        val (d, tmp2) = r / c
        val sNew = s / c
        val tNew = t / c
        (d, sNew._1, tNew._1, zeroPolynomial, zeroPolynomial, zeroPolynomial)
      }
    }
    val (gcdFinal,sFinal,tFinal, dummy1, dummy2, dummy3) = loop(r, s, t, rPrime, sPrime, tPrime)
    (gcdFinal, sFinal, tFinal)
  }

  def gcd(g: Polynomial, h: Polynomial): Polynomial = {
    val tmp = gcdExtended(g,h)
    tmp._1
  }

  def exp(h: Polynomial, exponent: Int): Polynomial = {
    def loop(h: Polynomial, exp: Int, acc: Polynomial): Polynomial =
      if (exp <= 1) acc else loop(h, exp - 1, acc * h)
    loop (h, exponent, h)
  }

/*  def isIrreducible(f: Polynomial): Boolean = {
    val limit: Int = f.degree / 2
    val q =
    val lista: List[Boolean] = List[Boolean]()
    for (k <- 1 to limit) {
      lista :: gcd()


    }
  }*/




  /**
   * builder of a Polynomial belonging to the polynomial ring
   */
  object Polynomial {

    def apply(map: T1): T2 = {

      val normalMap: T1 = {
        val theMapList = map.toList
        def newMapList(oldMapList: List[(Int, field.T2)]): List[(Int, field.T2)] = oldMapList match {
          case Nil => Nil
          case ((x1, field.zero) :: xs) => newMapList(xs)
          case ((x1, x2) :: xs)  => (x1, x2) :: newMapList(xs)
        }
        val theNewMapList = newMapList(theMapList)
        val theNewMap = theNewMapList.toMap
        theNewMap
      }
      new Polynomial(normalMap)
    }

    // Creo que esto es para comparar monomios de acuerdo con el grado
    def comp(monomial1: (Int, field.T2), monomial2: (Int, field.T2)) = monomial1._1 > monomial2._1

  }

  val zeroPolynomial: Polynomial = Polynomial(Map(0 -> field.zero))

  val x = Polynomial(Map(1 -> field.one))



  class Polynomial private(val map: T1)  {

    val fatherPolynomialOverField = PolynomialOverField.this

    def add(other: Polynomial) = {

      val tmp3 = field.zero
      val exponents = (map.keySet ++ other.map.keySet).toList
      def recursion(exp: List[Int]): Map[Int, field.T2] = exp match {
        case Nil => Map[Int, field.T2]()
        case x :: xs => recursion(xs) + (x -> (map.getOrElse(x, tmp3) + other.map.getOrElse(x, tmp3)))
      }
      val temporalMap = recursion(exponents)
      val temporalPoly = Polynomial(temporalMap)
      val result = temporalPoly
      result
    }

    def minus(other: Polynomial): Polynomial = {
      val tmp1 = other
      val tmp2 = field.one.negate
      val tmp3 = tmp1 * tmp2
      val tmp4 = this.add(tmp3)
      tmp4
    }

    def multiply(other: Polynomial): Polynomial = { // TODO
    val step1 = for (i <- this.map.toList; j <- other.map.toList) yield (i._1 + j._1, i._2 * j._2)
      val exponents = step1.map(x => x._1).distinct
      val step2 = for (i <- exponents) yield step1.filter(x => x._1 == i)

      def sumListInRing(list1: List[field.T2]): field.T2 = list1 match {
        case Nil => field.zero
        case x :: xs => x + sumListInRing(xs)
      }

      def sumCoef(l: List[(Int, field.T2)]): (Int, field.T2) = {
        (l.head._1, sumListInRing(l.map(x => x._2)))
      }

      val step3 = step2.map(sumCoef)
      val step4 = step3.toMap
      Polynomial(step4)

    }

    val degree: Int = {
      //val step1 =  map.keySet
      val step1 = map.keySet
      if (this.map == Map(0 -> field.zero) || this.map == Map[Int, field.T2]()) -999999 else step1.max
    }

    val lc: field.T2 = {
      if (degree == -999999) field.zero else this.map(degree)
    }

    // Ver https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Euclid.27s_algorithm
    def divide(other: Polynomial): (Polynomial, Polynomial) = {

      val a: Polynomial = this
      val b: Polynomial = other
      val d: Int = b.degree

      def s(r: Polynomial): Polynomial = Polynomial(Map( r.degree - d -> r.lc.divide(b.lc)))

      def loop(q: Polynomial, r: Polynomial): (Polynomial, Polynomial) = {
        if (r.degree < d) (q, r) else {
          loop(q + s(r), r - (s(r) * b))
        }
      }
      loop(zeroPolynomial, a)
    }

    def divide(other: field.T2): (Polynomial, Polynomial) = {

      val a: Polynomial = this
      val b: Polynomial = builder(Map(0 -> other))
      a.divide(b)

    }

    def multiply(other: field.T2): Polynomial = this.multiply(Polynomial(Map(0 -> other)))

    def *(other: Polynomial): Polynomial = this.multiply(other)
    def *(other: field.T2): Polynomial = this.multiply(other)

    def +(other: Polynomial): Polynomial = this.add(other)

    def -(other: Polynomial): Polynomial = this.minus(other)

    def /(other: Polynomial): (Polynomial, Polynomial) = this.divide(other)
    def /(other: field.T2): (Polynomial, Polynomial) = this.divide(other)

    def toMonic: Polynomial = {
      val oldMapList = map.toList
      def newMapList(oldMapList: List[(Int, field.T2)]): List[(Int, field.T2)] = oldMapList match {
        case Nil => Nil
        case ((x1, x2) :: xs)  => (x1, x2.divide(lc)) :: newMapList(xs)
      }
      if (lc == field.one) this else {
        val finalMap = newMapList(oldMapList).toMap
        new Polynomial(finalMap)
      }
    }

    def isMonic: Boolean = this == this.toMonic

    def mod(h: Polynomial): Polynomial = this.divide(h)._2

    def isIrreducible: Boolean = ???

    override def toString = {
      def printPol(a: List[(Int, field.T2)]): String = a match {
        case Nil => ""
        case x :: xs if x._1 == 0 => x._2 + " + " + printPol(xs)
        case x :: xs if x._1 == 1 && x._2 == field.one => "x" + " + " + printPol(xs)
        case x :: xs if x._1 == 1 => x._2 + "x" + " + " + printPol(xs)
        case x :: xs if x._2 == field.one => "x" + x._1 + " + " + printPol(xs)
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
        val a2: Boolean = (this.map == Map[Int, field.T2]()) && (that.map == Map(0 -> field.zero))
        val a3: Boolean = (that.map == Map[Int, field.T2]()) && (this.map == Map(0 -> field.zero))
        a1 || a2 || a3
      }
    }

  }
}

