package scala

import models.algebra._
import  Utils._
import scala.language.implicitConversions




object TestPolynomialOverRing extends App {

  println("Empezamos Polynomials Over a Field")

  val anillo = Zn(4)

  val GFdeX = PolynomialOverRing(anillo)

  implicit def c1(x: Int): GFdeX.ring.T1 = x.asInstanceOf[GFdeX.ring.T1]
  implicit def util(x: Int): GFdeX.ring.T2 = GFdeX.ring.builder(x)


  val aMap = Map( 3 -> util(3),
                  2 -> util(2),
                  1 -> util(1),
                  0 -> util(4))


  val a = GFdeX.builder(aMap)



  val b = a * a

  val Toma = PolynomialOverRing(GFdeX)

  println(b)

  val estoQueEs = a.fatherAbelianGroup
  val yEsto = a.fatherRing

  println(estoQueEs)
  println(yEsto)
  println()
  println()
  println()
  println(a.fatherPolynomialOverRing)
  println(a.fatherMagma)
  println(a.fatherRing)
  println(a.fatherSemigroup)
  println(a.fatherAbelianGroup)




}