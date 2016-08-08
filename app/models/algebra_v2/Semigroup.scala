package models.algebra_v2

trait Semigroup[T] {
  self =>
  def bin(f1: T, f2: T): T

  trait SemigroupLaw {
    def associative(f1: T, f2: T, f3: T): Boolean =
      bin(f1, bin(f2, f3)) == bin(bin(f1, f2), f3)
    def op(x1: T, x2: T) = bin(x1, x2)
  }

}

trait Monoid[T] extends Semigroup[T] {
  val identity: T
  trait MonoidLaw extends SemigroupLaw{

  }
}

trait Group[T] extends Monoid[T] {

  def bin(f1: T, f2: T): T

  trait GroupLaw extends MonoidLaw {
    def operation(x1: T, x2: T) = bin(x1, x2)
  }
  def inverse(x: T): T
}

trait AbelianGroup[T] extends Group[T] {

  trait AbelianGroupLaw extends GroupLaw {
    def commutative(f1: T, f2: T): Boolean =
      bin(f1, f2) == bin(f2, f1)
    def suma(x1: T, x2: T) = bin(x1, x2)

  }

  def inverse(x: T): T
}

trait Ring[T] extends Group[T] with AbelianGroup[T] {

  trait Add extends AbelianGroupLaw
  trait Multiply extends GroupLaw
}



object Test1 extends App {
  println("Empezamos")

  object Z extends Semigroup[Int] {
    def bin(f1: Int, f2: Int): Int = f1 + f2

    object suma extends SemigroupLaw

  }

  object Z2 extends Monoid[Int] {
    def bin(f1: Int, f2: Int): Int = f1 + f2
    val identity: Int = 0

    object suma extends MonoidLaw

  }

  object Z3 extends Group[Int] {
    def bin(f1: Int, f2: Int): Int = f1 + f2
    val identity: Int = 0
    def inverse(x: Int): Int = -x

    object suma extends GroupLaw

  }


  object Z4 extends Ring[Int] {
    def bin(f1: Int, f2: Int): Int = f1 * f2
    val identity: Int = 0
    def inverse(x: Int): Int = -x

    object suma extends Add {



    }
    object prod extends Multiply {

    }

  }



  val b = Z.bin(3, 4)
  println(b)

  println(Z.suma.associative(2, 3, 4))
  println(Z.suma.op(2, 3))

  val t = Z3.suma
}


