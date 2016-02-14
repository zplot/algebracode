package scala

import models.algebra._

object PruebaFp extends App {

  println("Empezamos")

  val cuerpo = Fp(43)
  val a = cuerpo.builder(1520)
  val b = cuerpo.builder(7870)


  println(a.minus(b))
  println(a*b)
  println(a.inverse*a)

  println(cuerpo.gcd(a, b))


}