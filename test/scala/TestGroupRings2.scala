package scala

import models.algebra._

import FiniteGroupExamples._



object TestGroupRings2 extends App {

  println("Empezamos TestGroupRings2")



  val group = S(3)
  val ring = Zn(6)


  val example = GroupRing(group, ring)

  val tres = ring.builder(3)

  val anillo = example.ring
  println(anillo)

  val tresp = anillo.builder(3)




}
