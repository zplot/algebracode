package scala

import models.algebra._

object PruebaUtils extends App {

  println("Empezamos")
  import Utils._




  println(primeFactors(12 * 64 * 9 * 49 * 121 * 2))

  println("Start testing toBinary")

  println(toBinary(0))
  println(toBinary(1))
  println(toBinary(2))
  println(toBinary(3))
  println(toBinary(4))
  println(toBinary(255))
  println(toBinary(456746576))
  println(toBinary(14))

  println()
  println("Start testing repeatedSquaring")

  println(power(3, 27))

  val ring = Zn(7)
  val a = ring.builder(5)
  val b = 3
  val c = a.power(b)
  println(c)








}