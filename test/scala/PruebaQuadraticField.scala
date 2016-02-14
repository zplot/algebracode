package scala

import models.algebra._

object PruebaQuadraticField extends App {

  println("Empezamos")

  val gauss = new QuadraticField(-1)
  val a = gauss.builder(1, 1)
  val b = gauss.builder(2, 2)


  println(a.minus(b))
  println(a*b)
  println(a*a)

}


