package scala

import models.algebra._

object TestFiniteGroups extends App {

  import FiniteGroupExamples._

  println("Empezamos")



  println()

  val s = "fS(4)"

  def result(s: String)  = {
    val t = fromStringToGroup(s)
      t match {
        case Right(x)  => x.cayleyTableOK
        case Left(x) => x
      }

  }

  println(result(s))

}