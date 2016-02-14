package scala

import models.algebra._
import  Utils._

object TestFiniteField extends App {

  println("Empezamos")



  val cuerpo = FiniteField(3,3)

  import cuerpo._

  def util(x: Int) = polyRing.field.builder(x)

  val aMap = Map(
    6 -> util(1),
    5 -> util(2),
    3 -> util(1),
    2 -> util(0),
    1 -> util(1),
    0 -> util(2))

  val bMap = Map(
    6 -> util(1),
    5 -> util(1),
    3 -> util(0),
    2 -> util(1),
    1 -> util(1),
    0 -> util(2))

  // Primero construimos en Polyring y después en cuerpo
  // Tambien se puede hacer algo del estilo:

  def constructor2(Map: polyRing.T1) = builder(polyRing.builder(Map))

  val c = constructor2(aMap)


  val aInPolyring = polyRing.builder(aMap)
  val bInPolyring = polyRing.builder(bMap)

  val a = builder(aInPolyring)
  val b = builder(bInPolyring)

  val aMapBis = IntMap(Map(
    6 -> 1,
    5 -> 2,
    3 -> 1,
    2 -> 0,
    1 -> 1,
    0 -> 2))

  val aBis = builder(aMapBis)

  println("¿aBis == a?")
  println(a == aBis)





  println("h = " + h)
  println("a = " + a)
  println("b = " + b)
  println("a + b = " + (a + b))
  println("a - b = " + (a - b))
  println("a * b = " + (a * b))
  println("inverse(a) = " + a.inverse)
  println("inverse(a) * a = " + a * a.inverse)
  println("a / b = " + a.divide(b))



  println("gcd(a,b) = " + gcd(a, b))


}