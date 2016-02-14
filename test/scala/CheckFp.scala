package scala

import models.algebra.Fp
import org.scalacheck._
import Prop.forAll

object CheckFp extends Properties("Fp") {

  lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
    ps.takeWhile{j => j * j <= i}.forall{ k => i % k > 0}) // Stream of primes



  val myGen = for {
    n <- Gen.choose(-10000,10000)
    m <- Gen.choose(-10000,10000)
    l <- Gen.choose(10, 14)
  } yield (n,m,ps(l))





  property("add") = forAll (myGen) { (n:(Int,Int,Int)) =>
    n match {
      case (n1, n2, n3) =>
        val Fpcase = Fp(n3)
        val a = Fpcase.builder(n1)
        val b = Fpcase.builder(n2)
        val c = Fpcase.builder(n1 + n2)
        a + b == c
    }
  }

  val ints = Gen.choose(-100, 100)


  property("subtract") = forAll (myGen) { (n:(Int,Int,Int)) =>
    n match {
      case (n1, n2, n3) =>
        val Fpcase = Fp(n3)
        Fpcase.builder(n1) - Fpcase.builder(n2) == Fpcase.builder(n1 - n2)
    }
  }

  property("multiply") = forAll (myGen) { (n:(Int,Int,Int)) =>
    n match {
      case (n1, n2, n3) =>
        val Fpcase = Fp(n3)
        Fpcase.builder(n1) * Fpcase.builder(n2) == Fpcase.builder(n1 * n2)
    }
  }

  val myGen2 = for {
    m <- Gen.choose(1,100)
    n <- Gen.choose(-10000,10000) suchThat (_ % ps(m) != 0) // To avoid zero
  } yield (n,ps(m))

  property("inverse") = forAll (myGen2) { (n:(Int,Int)) =>
    n match {
      case (n1, n2) =>
        val Fpcase = Fp(n2)
        Fpcase.builder(n1) * Fpcase.builder(n1).inverse == Fpcase.one
    }
  }

  val myInts2 = for {
    n <- Gen.choose(-10000,10000)
    m <- Gen.choose(-10000,10000)
  } yield (n,m)

  property("GCD") = forAll (myGen) { (n: (Int, Int, Int)) =>
    n match {
      case (n1, n2, n3) =>
        val Fpcase = Fp(n3)
        val a = Fpcase.builder(n1)
        val b = Fpcase.builder(n2)
        val d = Fpcase.gcd(a, b)
        val e = Fpcase.gcd(b, a)
        d == e
    }
  }
}