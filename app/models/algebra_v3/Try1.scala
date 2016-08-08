package models.algebra_v3

trait Semigroup[T] {
  def bin(x1: T, x2: T): T

  trait SemigroupLaw {
  }

}

trait Monoid[T] extends Semigroup[T] {
  val identity: T

  trait MonoidLaw extends SemigroupLaw {
  }

}

trait Group[T] extends Monoid[T] {
  def inverse(x: T): T

  trait GroupLaw extends MonoidLaw {
  }

}

trait AbelianGroup[T] extends Group[T] {

  trait AbelianGroupLaw extends GroupLaw {
  }

}

trait Ring[T] extends Group[T] with AbelianGroup[T] {
}

object Factories {
  def instanceMonoid[A](f: (A, A) => A, z: A): Monoid[A] = {
    new Monoid[A] {
      val identity = z
      def bin(f1: A, f2: A): A = f(f1, f2)
    }
  }
  def instanceRing[A](f: (A, A) => A, g: (A, A) => A, z: A, inv: A => A): Ring[A] = {
    new Ring[A] {
      val identity = z
      def inverse(x: A) = inv(x)
      def bin(f1: A, f2: A): A = f(f1, f2)
    }
  }
}

object Test1 extends App {
  println("Empezamos")
  def suma(x: Int, y: Int) = x + y
  def producto(x: Int, y: Int) = x * y

  import Factories._

  val monoid1 = instanceMonoid[Int]((x: Int, y: Int) => x + y, 0)
  val Z: Ring[Int] = instanceRing[Int]((x: Int, y: Int) => x + y, (x: Int, y: Int) => x * y, 0, (x: Int) => -x)
  val Z2: Ring[Int] = instanceRing[Int](
    (x: Int, y: Int) => x + y,
    (x: Int, y: Int) => x * y,
    0,
    (x: Int) => -x
  )
}




