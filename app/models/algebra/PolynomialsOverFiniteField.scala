package models.algebra
import models.algebra.Utils._


/**
  * builder of a T2 Ring over finite field
  */
object PolynomialsOverFiniteField {

  def apply(field: FiniteField): PolynomialsOverFiniteField = {
    new PolynomialsOverFiniteField(field)
  }

}


/**
  * builder of a T2 Ring over field
  */
class PolynomialsOverFiniteField private(val field: FiniteField) {

  def builderFromMap(map: field.polyRing.T1): field.FiniteFieldElement = field.builder(field.polyRing.builder(map))


  type T1 = Map[Int, field.T2]
  type T2 = Polynomial

  def builder(x: T1) = Polynomial(x)

  def findAllIrredPol(degree: Int): List[T2] = {

    def fromListToPoly(list: List[field.baseField.T2]): T2 = {

      val oneTwoThree = (0 to degree).toList
      val ourZip = oneTwoThree zip list
      val ourMap = ourZip.toMap.asInstanceOf[T1]
      builder(ourMap)
    }

    val listOfFieldElements = (0 to field.p - 1).toList map (x => field.baseField.builder(x))
    val listOflists = combinations[field.baseField.T2](degree + 1, listOfFieldElements)
    val listOflistsDrop0 = listOflists.tail
    val listOfPolys = listOflistsDrop0 map (x => fromListToPoly(x))
    val listMonic = listOfPolys.filter(_.isMonic)
    val listOfPolysDegree = listMonic.filter(_.degree == degree)
    val listOfIrreducibles = listOfPolysDegree.filter(_.isIrreducible)

    listOfIrreducibles
  }

  def findIrredPol(degree: Int): T2 = {

    findAllIrredPol(degree).head

  }

  def findIrredPolProb(degree: Int) = {

    def fromListToT2(list: List[Int]) = {
      val listOfElements = list map ( x => field.baseField.builder(x))
      val oneTwoThree = list.indices.toList.reverse
      val quasiMap = oneTwoThree zip listOfElements
      val map = quasiMap.toList.toMap.asInstanceOf[T1]
      builder(map)
    }

    val p = field.p

    def genT2 = {
      val sequence = for (i <- 1 to degree) yield randomP(p)
      val list = sequence.toList
      val listExtended = 1 :: list
      val T2 = fromListToT2(listExtended)
      T2
    }

    def loop: T2 = {
      val generated = genT2
      if (generated.isIrreducible) {
        generated
      } else {
        loop
      }
    }

    loop
  }

  // See Victor Shoup pag. 472
  def gcdExtended(g: T2, h: T2): (T2, T2, T2) = {

    val r: T2 = g
    val rPrime: T2 = h
    val s: T2 = builder(Map(0 -> field.one))
    val sPrime: T2 = zero
    val t: T2 = zero
    val tPrime: T2 = builder(Map(0 -> field.one))

    def loop(r: T2, s: T2, t: T2, rPrime: T2, sPrime: T2,tPrime: T2): (T2, T2, T2,T2, T2, T2) = {

      if (rPrime != zero) {
        val (q, rPrimePrime) = r / rPrime
        loop(rPrime, sPrime, tPrime, rPrimePrime, s - sPrime * q, t - tPrime * q)
      } else {
        val c = r.lc
        val (d, _) = r / c
        val sNew = s / c
        val tNew = t / c
        (d, sNew._1, tNew._1, zero, zero, zero)
      }
    }
    val (gcdFinal,sFinal,tFinal, dummy1, dummy2, dummy3) = loop(r, s, t, rPrime, sPrime, tPrime)
    (gcdFinal, sFinal, tFinal)
  }

  def gcd(g: T2, h: T2): T2 = {
    val tmp = gcdExtended(g,h)
    tmp._1
  }

  // TODO Hacer esto con el Squering algorithm
  def exp(h: T2, exponent: Int): T2 = {

    def loop(h: T2, exp: Int, acc: T2): T2 = {
      if (exp <= 1) acc else loop(h, exp - 1, acc * h)
    }

    loop (h, exponent, h)
  }

  val zero: T2 = builder(Map(0 -> field.zero))
  val x = builder(Map(1 -> field.one))
  val one = builder(Map(0 -> field.one))


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

    def buildFromMap(map: field.polyRing.T1): T2 = {
      val polyOverFp = field.polyRing.builder(map).asInstanceOf[T1]
      new Polynomial(polyOverFp)
    }

    // Creo que esto es para comparar monomios de acuerdo con el grado
    def comp(monomial1: (Int, field.T2), monomial2: (Int, field.T2)) = monomial1._1 > monomial2._1

  }


  class Polynomial (val mapa: T1)  {

    val fatherPolynomialOverFiniteField = PolynomialsOverFiniteField.this

    val isZero = {
      val map = this.mapa
      val mapKeySet = map.keySet
      val mapValueSet = map.values.toSet
      val c0: Boolean = this == zero
      val c1: Boolean = mapKeySet == Set(0)
      val c2: Boolean = mapValueSet == Set(field.zero)
      c0 || (c1 && c2)
    }

    val degree: Int = {
      val exponents = mapa.keySet
      val coefficients = mapa.values.toSet
      val areAllCoeffsZero: Boolean = coefficients.toList.forall(x => x.isZero)
      val cond1 = this.mapa == Map(0 -> field.zero)
      val cond2 = this.mapa == Map[Int, field.T2]()
      val cond3 = areAllCoeffsZero
      val elgradoes = if (cond1 || cond2 || cond3)  -1 else exponents.max // TODO quitar esto
      if (cond1 || cond2 || cond3)  -1 else exponents.max
    }


    val lc = if (degree == -1) field.zero else {
      val list = mapa.toList
      val deg = mapa.keySet.max
      val tmp1 = list.filter(x => x._1 == deg)
      val tmp2 = tmp1.head
      val tmp3 = tmp2._2

      tmp3
    }

    val isMonic: Boolean = lc == field.one

    def add(other: T2) = {

      val tmp3 = field.zero
      val exponents = (mapa.keySet ++ other.mapa.keySet).toList
      def recursion(exp: List[Int]): Map[Int, field.T2] = exp match {
        case Nil => Map[Int, field.T2]()
        case x :: xs => recursion(xs) + (x -> (mapa.getOrElse(x, tmp3) + other.mapa.getOrElse(x, tmp3)))
      }

      val temporalMap = recursion(exponents)
      val temporalPoly = builder(temporalMap)
      val result = temporalPoly
      result
    }

    def minus(other: T2): T2 = {
      val tmp1 = other
      val tmp2 = field.one.negate
      val tmp3 = tmp1 * tmp2
      val tmp4 = this.add(tmp3)
      tmp4
    }

    def multiply(other: T2): T2 = { // TODO
    val step1 = for (i <- this.mapa.toList; j <- other.mapa.toList) yield (i._1 + j._1, i._2 * j._2)
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
      builder(step4)

    }

    // Ver https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Euclidean_algorithm
    def divide(other: T2): (T2, T2) = { // TODO Revisar esto porque parece que no va bien

      val a: T2 = this
      val b: T2 = other
      val d: Int = b.degree

      def s(r: T2): T2 = builder(Map( r.degree - d -> r.lc.divide(b.lc)))

      def loop(q: T2, r: T2): (T2, T2) = {
        if (r.degree < d) (q, r) else {
          loop(q + s(r), r - (s(r) * b))
        }
      }
      loop(zero, a)
    }

    def divide(other: field.T2): (T2, T2) = {

      val a: T2 = this
      val b: T2 = builder(Map(0 -> other))
      a.divide(b)

    }

    def multiply(other: field.T2): T2 = this.multiply(builder(Map(0 -> other)))

    def *(other: T2): T2 = this.multiply(other)
    def *(other: field.T2): T2 = this.multiply(other)

    def +(other: T2): T2 = this.add(other)

    def -(other: T2): T2 = this.minus(other)

    def /(other: T2): (T2, T2) = this.divide(other)
    def /(other: field.T2): (T2, T2) = this.divide(other)

    def toMonic: T2 = {
      val oldMapList = mapa.toList
      def newMapList(oldMapList: List[(Int, field.T2)]): List[(Int, field.T2)] = oldMapList match {
        case Nil => Nil
        case ((x1, x2) :: xs)  => (x1, x2.divide(lc)) :: newMapList(xs)
      }
      if (lc == field.one) this else {
        val finalMap = newMapList(oldMapList).toMap
        new T2(finalMap)
      }
    }



    def mod(h: T2): T2 = this.divide(h)._2

    //  Cohen page 127
    def isIrreducible: Boolean = {

      val n = degree
      val cond1: Boolean = {

        val exponent = math.pow(field.p, degree).toInt
        val xToPn = exp(x, exponent)
        //xToPn.mod(this) == x.mod(this)
        val xToPnMinusX = xToPn - x
        xToPnMinusX.mod(this) == zero
      }

      val cond2: Boolean = {
        val factores = factors(n)
        val factors2 = factores.slice(1, factores.length - 1)
        val tmp1 = for(q <- factors2) yield {
          val exp1 = n/q
          val tmp2 = exp(x, exp1)
          val tmp2MinusX = tmp2 - x
          val tmp4 = gcd(tmp2MinusX, this)
          val tmp5 = tmp4 == one
          tmp5
        }
        tmp1.forall(x => x)
      }

      cond1 && cond2
    }

    override def toString = {
      def printPol(a: List[(Int, field.T2)]): String = a match {
        case Nil => ""
        case x :: xs if x._1 == 0 => x._2 + " + " + printPol(xs)
        case x :: xs if x._1 == 1 && x._2 == field.one => "x" + " + " + printPol(xs)
        case x :: xs if x._1 == 1 => x._2 + "x" + " + " + printPol(xs)
        case x :: xs if x._2 == field.one => "x" + x._1 + " + " + printPol(xs)
        case x :: xs => x._2 + "x" + x._1 + " + " + printPol(xs)
      }
      if (this == zero) "0" else {
        val text = printPol(this.mapa.toList.sortWith(Polynomial.comp)).dropRight(2)
        val finalTex = text + "where h = " + field.h
        finalTex
      }
    }

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[T2]
      if (that == null) false
      else {
        val a1: Boolean = this.mapa == that.mapa
        val a2: Boolean = (this.mapa == Map[Int, field.T2]()) && (that.mapa == Map(0 -> field.zero))
        val a3: Boolean = (that.mapa == Map[Int, field.T2]()) && (this.mapa == Map(0 -> field.zero))
        a1 || a2 || a3
      }
    }

  }
}

