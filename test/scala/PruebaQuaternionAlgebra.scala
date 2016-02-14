package scala

import models.algebra._

object PruebaQuaternionAlgebra extends App {

  println("Empezamos")

  val qa = QuaternionAlgebra(Fp(3))

  def rin(x: Int): qa.ring.T2 = qa.ring.builder(x.asInstanceOf[qa.ring.T1])

  val qAlgebra = qa.QAab(rin(-1), rin(-1))

  val i = qAlgebra.builder(rin(0), rin(1), rin(0), rin(0))
  val menosUno = i * i
  val j = qAlgebra.builder(rin(0), rin(0), rin(1), rin(0))

  println(i)
  println(menosUno)
  println("i * j = k = " + i  * j)
  println("j * i = -k = " + j  * i)
  println(qAlgebra.zero)
  println(qAlgebra.one)
  println(qAlgebra.one + qAlgebra.one)

  val z1234 = qAlgebra.builder(rin(1), rin(2), rin(3), rin(4))
  println("z1234 = " + z1234)
  println("Conjugate of z1234 = " + qAlgebra.conjugate(z1234))
  println("Norm of z1234 = " + qAlgebra.norm(z1234))
  println("Norm of i + j = " + qAlgebra.norm(i + j))

  val qjmask = qAlgebra.builder(rin(0), rin(1), rin(1), rin(1))
  val qunomas2j = qAlgebra.builder(rin(0), rin(2), rin(2), rin(2))
  println("producto = " + qjmask * qunomas2j)

  val v1 = qAlgebra.builder(rin(0), rin(1), rin(1), rin(1))
  val v2 = qAlgebra.builder(rin(0), rin(1), rin(0), rin(1))

  println(v1 == v2)

}