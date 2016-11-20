package scala

import models.algebra._

import FiniteGroupExamples._
import scala.language.implicitConversions



object PruebaTodo extends App {

  println("Empezamos2 PruebaTodo")

  val cinco = Z.builder(5)
  val tres = Zn(6).builder(3)

  val group = S(3)
  val anillo: Zn = Zn(6)

  val example = GroupRing2(group, anillo)

  val hola = GroupRing2(group, anillo)
  val m1 = Z.builder(6)
  println(m1)

  val b1: Ring = hola.ring

  val r1 = hola.ring.one
  val r2 = hola.ring.builder(3.asInstanceOf[hola.ring.T1])
  val r3 = anillo.builder(3)
  val r4 = 3.asInstanceOf[hola.ring.T1]

  val r6 = anillo.builder(3)





}
