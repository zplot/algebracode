package scala

import models.algebra._

import FiniteGroupExamples._
import scala.language.implicitConversions



object PruebaGroupGroupRings extends App {

  println("Empezamos2")


  val cinco = Z.builder(5)
  val cuatro = Z.builder(4)
  println("5 + 4 = " + cinco.add(cuatro))
  println("5 * 4 = " + cinco.multiply(cuatro))

  val Z6 = Zn(6)
  val cincoMod6 = Z6.builder(5)
  println("5 * 5 = " + cincoMod6.multiply(cincoMod6))
  val w1 = Permutation(Set(Cycle(List(1, 2, 3))))
  val w2 = Permutation(Set(Cycle(List(1, 2))))

  val grupito = S(3)
  val anillo = Zn(6)

  val example = GroupRing2(grupito, anillo)

  val S3Zn6 = GroupRing2(grupito, anillo)

  val otroGrupo = S(4)

  val S4S3Zn6 = GroupRing2(otroGrupo, S3Zn6)

  println(S3Zn6)
  println(S4S3Zn6)




}
