// Este programa es una plantilla de cómo ir extendiendo estructuras algebraicas

package models.algebra


object Template {

  trait A {
    type T2 <: AElement
    trait AElement {
      def operation1(other: T2): T2
    }
  }

  trait B extends A {
    type T2 <: BElement
    trait BElement extends AElement {
      def operation2(other: T2): T2
    }
  }

  trait C extends B {
    type T2 <: CElement
    trait CElement extends BElement {
      def operation3(other: T2): T2
    }
  }

  trait D extends C {
    type T2 <: DElement
    trait DElement extends CElement {
      def operation4(other: T2): T2
    }
  }







  class Aexample extends A {
    type T2 = AExampleElement
    class AExampleElement extends AElement {
      def operation1(other: T2): T2 = other
    }
  }

  class Bexample extends B {
    type T2 = BExampleElement
    class BExampleElement extends BElement {
      def operation1(other: T2): T2 = other
      def operation2(other: T2): T2 = other
    }
  }

  class Cexample extends C {
    type T2 = CExampleElement
    class CExampleElement extends CElement {
      def operation1(other: T2): T2 = other
      def operation2(other: T2): T2 = other
      def operation3(other: T2): T2 = other
    }
  }

  class Dexample extends D {
    type T2 = DExampleElement
    class DExampleElement extends DElement {
      def operation1(other: T2): T2 = other
      def operation2(other: T2): T2 = other
      def operation3(other: T2): T2 = other
      def operation4(other: T2): T2 = other
    }
  }



}
