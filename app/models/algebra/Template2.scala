package models.algebra

import scala.language.implicitConversions


object Template2 {

  trait A {
    type T1
    type T2 <: AElement
    def builder(x: T1): T2

    trait AElement {
      def operation1(other: T2): T2
    }
  }

  trait B {
    type T1
    type T2 <: BElement
    def builder(x: T1): T2

    trait BElement  {
      def operation2(other: T2): T2
    }
  }

  case class Combi(grupo: A, anillo: B) extends B {
    type T1 = Vector[anillo.T2]
    type T2 = CombiElement
    def builder(x: T1): CombiElement = CombiElement(x)

    case class CombiElement(x: T1) extends BElement {
      def operation2(other: T2):CombiElement = other
    }
  }

  class Aexample extends A {
    type T1 = Int
    type T2 = AExampleElement
    def builder(x: T1): T2 = AExampleElement(5)
    case class AExampleElement(z: Int) extends AElement {
      def operation1(other: T2): T2 = other
    }
  }

  class Bexample extends B {
    type T1
    type T2 = BExampleElement
    def builder(x: T1): T2 = BExampleElement(9)
    case class BExampleElement(z: Int) extends BElement {
      def operation1(other: T2): T2 = other
      def operation2(other: T2): T2 = other
    }
  }



  val estructura1 = new Aexample
  val estructura2 = new Bexample
  val combinado = Combi(estructura1, estructura2)



  implicit def converter(x: Int): combinado.anillo.T1 = x.asInstanceOf[combinado.anillo.T1]

  val z1 = combinado.anillo.builder(3)







}
