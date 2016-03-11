package scala

import models.algebra._

object TestFiniteGroups extends App {

  import FiniteGroupExamples._

  println("Empezamos")

  val grupo = DirectProduct(S(3), Q8)

  println(grupo.cayleyTableOK)

  println()

  val s = "DirectProduct(Sp(3), Q8)"

  def result(s: String)  = {
    val t = fromStringToGroup2(s)
      t match {
        case Some(x)  => x.cayleyTableOK
        case None => "None"
      }

  }

  println(result(s))

}