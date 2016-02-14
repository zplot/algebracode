package scala

import models.algebra._
import  Utils._




object TestPolynomialsOverFp extends App {

  println("Empezamos Polynomials Over a Field")

  val cuerpo = Fp(5)
/*  val a = cuerpo.builder(1520)
  val b = cuerpo.builder(7870)*/




  val GFdeX = PolynomialsOverFp(cuerpo)

  val prueba = GFdeX.field.builder(7)

  def util(x: Int) = GFdeX.field.builder(x)
/*
  val tmp1 = util(5)

  val p1 = GFdeX.Polynomial(Map(0 -> util(5)))
  val p2 = GFdeX.Polynomial(Map(
    1 -> util(1),
    3 -> util(1),
    4 -> util(1)))

  val p3 = GFdeX.Polynomial(Map(
    0 -> util(1),
    1 -> util(1)))

  println("p2 = " + p2)
  println("p3 = " + p3)

  val p4 = p2.divide(p3)
  println("p2/p3 = " + p4)

  val p10 = GFdeX.Polynomial(Map(
    3 -> util(13),
    2 -> util(4),
    1 -> util(1),
    0 -> util(3)))

  val p20 = GFdeX.Polynomial(Map(
    3 -> util(0),
    8 -> util(9),
    1 -> util(9),
    0 -> util(0)))


  println("Ahora empieza GCD")
  println(p10)
  println(p20)
  println(GFdeX.gcdExtended(p10, p20))
  println(GFdeX.gcd(p10, p20))

  println("Ahora empieza exponentes")
  println(p10)
  val m = GFdeX.exp(p10, 3)
  println(m)

  println("Ahora empieza toMonic")
  println(p10)
  val m3 = p10.toMonic
  println(m3)
  println(p10.isMonic)
  println(m3.isMonic)


  println("Ahora empieza factors")
  println(factors(87775))

  println()*/
  println("Ahora empieza isIrreducible")
  println()
  val f = GFdeX.builder(Map(
    6 -> util(1),
    5 -> util(1),
    3 -> util(0),
    2 -> util(1),
    1 -> util(1),
    0 -> util(2)))
  println(f + "is irreducible == " + f.isIrreducible)

  println()
  println("Ahora empieza findIrredPol")
  println()
  val g = GFdeX.findIrredPol( 3)
  println(g.isIrreducible)
  println(g)


  val all = GFdeX.findAllIrredPol(3)
  println(all)

  println()
  println("Ahora empieza findIrredPolProb")
  println()
  val h = GFdeX.findIrredPolProb(5)
  println(h.isIrreducible)
  println(h)


  println()
  println("Ahora empieza findIrredPolProb dentro de la clase")
  println()
  val h2 = GFdeX.findIrredPolProb(5)
  println(h2.isIrreducible)
  println(h2)

  println()
  println("Ahora empieza pruebas de builder")
  println()

  val mapa = IntMap(Map(
    6 -> 1,
    5 -> 1,
    3 -> 0,
    2 -> 1,
    1 -> 1,
    0 -> 2))

  val fBis = GFdeX.builder(mapa)
  println(f)
  println(fBis)
  println(f == fBis)








}