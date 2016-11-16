package scala

import models.algebra._

import FiniteGroupExamples._



object PruebaGroupRings extends App {

  println("Empezamos2")

  val cinco = Z.builder(5)
  val cuatro = Z.builder(4)
  println("5 + 4 = " + cinco.add(cuatro))
  println("5 * 4 = " + cinco.multiply(cuatro))

  val Z6 = Zn(6)
  val cincoMod6 = Z6.builder(5)
  println("5 * 5 = " + cincoMod6.multiply(cincoMod6))
  val w1 = Permutation(Set(Cycle(List(1, 2, 3))))
  val w2 = Permutation(Set(Cycle(List(1, 2))))

  val group = S(3)
  val ring = Zn(6)

  val example = GroupRing(group, ring)




  val hola = GroupRing(group, ring)
  val m1 = Z.builder(6)
  println(m1)


  val r1 = hola.ring.one
  val r2 = hola.ring.builder(3.asInstanceOf[hola.ring.T1])


  println(r2)


  def r(x: Int) = hola.ring.builder(x.asInstanceOf[hola.ring.T1])

  println(r(4))


  val a1 = hola.builder(Vector(r(1)))

  val e1 = hola.builder(Vector(r(0), r(1), r(0), r(0), r(0), r(0)))
  val e2 = hola.builder(Vector(r(0), r(0), r(1), r(0), r(0), r(0)))

  println(e1)
  println(e2)

  val suma = e1.add(e2)
  println(suma)

  val producto = e1.multiply(e2)
  val producto2 = e1 + e2
  println(producto)

  val a = hola.builder(Vector(r(0), r(0), r(0), r(0), r(0), r(1)))
  val b = hola.builder(Vector(r(0), r(0), r(0), r(1), r(0), r(0)))

  val c = a * b + a
  println(c)

  val eta = a - a * a + a * b - a * a * b
  val etaCuadrado = eta * eta
  println("eta = " + eta)
  println("etaCuadrado = " + etaCuadrado)


  val d = a * r(4)
  // val e = gen(4) * a TODO
  println("a = " + a)
  println("b = " + b)
  println("a * a = " + a * a)
  println("a * b = " + a * b)
  println("a * a * b = " + (a * a) * b)
  println("a * a * b = " + a * (a * b))

  println("Prueba de GroupRings con Zn")

  val zw1 = Permutation(Set(Cycle(List(1, 2, 3))))
  val zw2 = Permutation(Set(Cycle(List(1, 2))))


  val znring1 = new Zn(12)
  val zngroup1 = PermutationGroup(Set(zw1, zw2))
  val adios = GroupRing(zngroup1, znring1)


  def rb(x: Int) = adios.ring.builder(x.asInstanceOf[adios.ring.T1])

  val zqna = adios.GroupRingElement(Vector(rb(0), rb(0), rb(0), rb(0), rb(0), rb(1)))
  val zqnb = adios.GroupRingElement(Vector(rb(0), rb(0), rb(0), rb(1), rb(0), rb(0)))

  val zqnc = zqna * zqnb + zqna
  println(zqnc)


  val zqneta = zqna - zqna * zqna + zqna * zqnb - zqna * zqna * zqnb
  val zqnetaCuadrado = zqneta * zqneta
  println("zqneta = " + zqneta)
  println("zqnetaCuadrado = " + zqnetaCuadrado)


}
