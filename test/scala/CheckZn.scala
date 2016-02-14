package scala

import models.algebra.Zn
import org.scalacheck._
import Prop.forAll

object CheckZn extends Properties("Zn") {


  val myGen = for {
    n <- Gen.choose(-10000,10000)
    m <- Gen.choose(-10000,10000)
    l <- Gen.choose(1, 1000)
  } yield (n,m,l)

  property("add") = forAll (myGen) { (n:(Int,Int,Int)) =>
    n match {
      case (n1, n2, n3) =>
        val Zcase = Zn(n3)
        Zcase.builder(n1) + Zcase.builder(n2) == Zcase.builder(n1 + n2)
    }
  }

  val ints = Gen.choose(-100, 100)

  property("zero - x") = forAll(ints) { d =>
    val Z89 = Zn(89)
    Z89.zero - Z89.builder(d) == Z89.builder(d).negate
  }

  property("subtract") = forAll (myGen) { (n:(Int,Int,Int)) =>
    n match {
      case (n1, n2, n3) =>
        val Zcase = Zn(n3)
        Zcase.builder(n1) - Zcase.builder(n2) == Zcase.builder(n1 - n2)
    }
  }

  property("multiply") = forAll (myGen) { (n:(Int,Int,Int)) =>
    n match {
      case (n1, n2, n3) =>
        val Zcase = Zn(n3)
        Zcase.builder(n1) * Zcase.builder(n2) == Zcase.builder(n1 * n2)
    }
  }


  property("builder") = forAll { a: Int =>
    val Z89 = Zn(89)
    Z89.builder(a)  == Z89.IntModN(a)
  }
}