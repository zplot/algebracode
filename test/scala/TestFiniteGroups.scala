package scala

import models.algebra._

object TestFiniteGroups extends App {

  import FiniteGroupExamples._

  println("Empezamos")

  val grupo = DirectProduct(S(3), Q8)

  println(grupo.cayleyTableOK)

  println()

  val s = "DirectProduct(S(3), D(3))"

  def result(s: String)  = {
    val t = fromStringToGroup(s)
      t match {
        case Right(x)  => x.cayleyTableOK
        case Left(x) => x
      }

  }

  println(result(s))

}