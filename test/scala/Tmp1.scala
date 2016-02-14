package scala

import models.algebra._

import Utils._


object Tmp1 extends App {

  println("Empezamos Polynomials Over a Field")

  val cuerpo = FiniteField(2,5)

  val GFdeX = PolynomialsOverFiniteField(cuerpo)


  // GFdeX es el anillo de polinomios sobre el cuerpo finito
  // GFdeX.field el el cuerpo finito FiniteField(2,5)
  // GFdeX.field.baseField el el cuerpo Fp, en este caso F2
  // GFdeX.field.polyRing es PolynomialsOverFp(baseField)
  // aMap es un Map[Int, FpElement]
  // aPoly es un polinomio en GFdeX.field.polyRing, es decir, en PolynomialsOverFp(GFdeX.field.baseField)
  // bPoly es un polinomio en GFdeX.field.polyRing, es decir, en PolynomialsOverFp(GFdeX.field.baseField)
  // map1 es un Map[Int, polyRing.T2]
  // map2 es un Map[Int, polyRing.T2]
  // pol1 es un polinomio de los que buscamos
  // pol2 es un polinomio de los que buscamos




  def cons1(x: Int) = GFdeX.field.baseField.builder(x)

  val aMap = Map(
    6 -> cons1(1),
    5 -> cons1(2),
    3 -> cons1(1),
    2 -> cons1(0),
    1 -> cons1(1),
    0 -> cons1(2)).asInstanceOf[GFdeX.field.polyRing.T1]

  val bMap = Map(
    6 -> cons1(1),
    5 -> cons1(1),
    3 -> cons1(0),
    2 -> cons1(1),
    1 -> cons1(1),
    0 -> cons1(2)).asInstanceOf[GFdeX.field.polyRing.T1]

  val aPoly = GFdeX.field.polyRing.builder(aMap)
  val bPoly = GFdeX.field.polyRing.builder(bMap)

  def constructor2(Map: GFdeX.field.polyRing.T1) = GFdeX.field.builder(GFdeX.field.polyRing.builder(Map))




  val map1 = Map(
    6 -> aPoly,
    5 -> aPoly,
    3 -> aPoly,
    2 -> aPoly,
    1 -> aPoly,
    0 -> aPoly).asInstanceOf[GFdeX.T1]

  val map2 = Map(
    6 -> bPoly,
    5 -> bPoly,
    3 -> bPoly,
    2 -> bPoly,
    1 -> bPoly,
    0 -> bPoly).asInstanceOf[GFdeX.T1]

  val pol1 = GFdeX.builder(map1)
  val pol2 = GFdeX.builder(map2)

  def cons2(Map: GFdeX.field.polyRing.T1) = GFdeX.field.builder(GFdeX.field.polyRing.builder(Map)).asInstanceOf[GFdeX.field.polyRing.T1]

  def cons3(Map: GFdeX.T1) = GFdeX.builder(Map)




  val a = pol1
  val b = pol2

  println("Ahora empieza GCD")
  println(a)
  println(b)
  println(GFdeX.gcdExtended(a, b))
  println(GFdeX.gcd(a, b))

  println("Ahora empieza exponentes")
  println(a)
  val m = GFdeX.exp(a, 3)
  println(m)

  println("Ahora empieza toMonic")
  println(a)
  val m3 = a.toMonic
  println(m3)
  println(a.isMonic)
  println(m3.isMonic)


  println("Ahora empieza factors")
  println(factors(87775))

  println()
  println("Ahora empieza isIrreducible")
  println()




  println(pol1.isIrreducible)
  println(pol2.isIrreducible)





}