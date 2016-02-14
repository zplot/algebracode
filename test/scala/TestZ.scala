package scala

import models.algebra._


object TestZ extends App {

  println("Empezamos")
  val a = Z.builder(1212121)
  val b = Z.builder(24)
  val c = Z.gcd(a,b)

  println(a,b,c)
}
