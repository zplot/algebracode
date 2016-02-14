package scala

import models.algebra._

object TestFiniteGroups extends App {

  import FiniteGroupExamples._

  println("Empezamos")

  val grupo = S(3)

  println(grupo.cayleyTableOK)


}