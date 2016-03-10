package scala

import models.algebra._

object TestFiniteGroups extends App {

  import FiniteGroupExamples._

  println("Empezamos")

  val grupo = DirectProduct(S(3), Q8)

  println(grupo.cayleyTableOK)

  println()

  val s = "DirectProduct(S(3), Q8)"

  println(fromStringToGroup(s).cayleyTableOK)


}