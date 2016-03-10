package scala

import models.algebra._

object TestFiniteGroups extends App {

  import FiniteGroupExamples._

  println("Empezamos")

  val grupo = S(4)

  println(grupo.cayleyTableOK)

  println()

  val s = "D(4)"

  println(fromStringToGroup(s).cayleyTableOK)


}