package scala

import models.algebra._

object QAExperiment1 extends App {

  println("Empezamos QAExperiment1")

  val p = 3
  val qa = QuaternionAlgebra(Fp(p))

  // rin convierte un entero en el correspondiente elemento del anillo
  def rin(x: Int): qa.ring.T2 = qa.ring.builder(x.asInstanceOf[qa.ring.T1])

  // qAlgebra el el Quaternion Algebra que vamos a estudiar
  val qAlgebra = qa.QAab(rin(-1), rin(-1))



  val all = {
    for (
      x1 <- 0 to p - 1;
      x2 <- 0 to p - 1;
      x3 <- 0 to p - 1;
      x4 <- 0 to p - 1
    ) yield {
      (rin(x1), rin(x2), rin(x3), rin(x4))
    }
  }

  val allExceptZero = all.filterNot(x => x == (qa.ring.zero, qa.ring.zero, qa.ring.zero, qa.ring.zero))

  var zeroDivisors = List[qAlgebra.Quaternion](qAlgebra.zero)
  val products = {
    for (i <- allExceptZero;
         j <- allExceptZero) yield {
      if (qAlgebra.builder(i) * qAlgebra.builder(j) == qAlgebra.zero) {
/*      println()
        println(qAlgebra.builder(i))
        println(qAlgebra.builder(j))
        val tmp1: qAlgebra.Quaternion = qAlgebra.builder(i)
        val tmp2: qAlgebra.Quaternion = qAlgebra.builder(j)
        println(tmp1 == tmp2)
        println(zeroDivisors)*/
        if (!zeroDivisors.contains(qAlgebra.builder(i))) {
          zeroDivisors = qAlgebra.builder(i) :: zeroDivisors
        }
        if (!zeroDivisors.contains(qAlgebra.builder(j))) {
          zeroDivisors = qAlgebra.builder(j) :: zeroDivisors
        }
      }
    }
  }

  println(zeroDivisors)
  println(zeroDivisors.size)
}

