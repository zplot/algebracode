package scala

import models.algebra._

import FiniteGroupExamples._
import scala.language.implicitConversions



object TestGroupRings2 extends App {

  println("Empezamos TestGroupRings2")



  val s3 = S(3)
  val zn6 = Zn(6)


  val groupRing1 = GroupRing(s3, zn6)

  val tres = zn6.builder(3)

  val anillo = groupRing1.ring
  println(anillo)

  implicit def conversor(x: Int): anillo.T1 = x.asInstanceOf[anillo.T1]


  val tresp = anillo.builder(3)

  val tresp2 = anillo.builder(4)




}
