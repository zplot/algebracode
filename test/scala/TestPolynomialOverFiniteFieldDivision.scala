package scala

import models.algebra._

import Utils._


object TestPolynomialOverFiniteField2 extends App {

  println("Empezamos Polynomials Over a Field 2")

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
  println("1")

  val aMap = Map(

    3 -> cons1(0),
    2 -> cons1(1),
    1 -> cons1(1),
    0 -> cons1(2)).asInstanceOf[GFdeX.field.polyRing.T1]
  println("2")

  val bMap = Map(

    3 -> cons1(0),
    2 -> cons1(1),
    1 -> cons1(1),
    0 -> cons1(2)).asInstanceOf[GFdeX.field.polyRing.T1]
  println("3")

  //val aPoly = GFdeX.field.polyRing.builder(aMap)
  println("4")
  //val bPoly = GFdeX.field.polyRing.builder(bMap)
  println("5")

  def constructor2(Map: GFdeX.field.polyRing.T1) = GFdeX.field.builder(GFdeX.field.polyRing.builder(Map))
  println("6")

  val aPoly = constructor2(aMap)
  val bPoly = constructor2(bMap)


  val map1 = Map(

    2 -> aPoly,
    1 -> aPoly,
    0 -> aPoly)
  println("7")

  val map2 = Map(

    1 -> bPoly,
    0 -> bPoly)
  println("8")

  val pol1 = GFdeX.builder(map1)
  println("9")
  val pol2 = GFdeX.builder(map2)
  println("10")

  def cons2(Map: GFdeX.field.polyRing.T1) = GFdeX.field.builder(GFdeX.field.polyRing.builder(Map)).asInstanceOf[GFdeX.field.polyRing.T1]
  println("11")
  def cons3(Map: GFdeX.T1) = GFdeX.builder(Map)
  println("12")

  println("Ahora probamos el nuevo builder")

  val mapa1 = Map(

    3 -> cons1(1),
    2 -> cons1(0),
    1 -> cons1(1),
    0 -> cons1(2)).asInstanceOf[GFdeX.field.polyRing.T1]

  val elemento = GFdeX.builderFromMap(mapa1)
  println("elemento = " + elemento)


  println()
  println()
  println("Ahora empieza División de a entre b")
  println()



  val a = pol1
  val b = pol2


  println("a = " + a)
  println("b = " + b)

  val c = a / b

  println("a / b = " + c)


}