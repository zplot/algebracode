

import models.algebra._

import FiniteGroupExamples._



object TestGroupRings2 {

  println("Empezamos TestGroupRings2")



  val group = S(3)
  val ring = Zn(6)

  val groupRing1 = GroupRing(group, ring)


  val uno = groupRing1.ring.one
  val dos = groupRing1.ring.builder(2)
  val tres = groupRing1.ring.builder(3.asInstanceOf[groupRing1.ring.T1])

  uno

  println(group)
  println(ring)
  println(groupRing1)
  println(uno)
  println(dos)
  println(tres)


}