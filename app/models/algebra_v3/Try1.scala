package models.algebra_v3


trait Semigroup2[T] {

  def bin(x1: T, x2: T): T

  trait Semigroup2Law {

  }

}



trait Monoid[T] extends Semigroup2[T]  {
  val identity: T
  trait MonoidLaw extends Semigroup2Law {

  }
}

trait Group[T] extends Monoid[T] {

  def inverse(x: T): T

  trait GroupLaw extends MonoidLaw {

  }

}

trait AbelianGroup[T]  extends Group[T] {


  trait AbelianGroupLaw extends GroupLaw {

  }
}

trait Ring[T] extends Group[T] with AbelianGroup[T] {


}





object Test1 extends App {

  println("Empezamos")

  def suma(x: Int, y: Int) = x + y
  def producto(x: Int, y: Int) = x * y

  object Z extends Ring[Int]  {

    def bin(f1: Int, f2: Int): Int = f1 + f2
    val identity: Int = 0
    def inverse(x: Int): Int = -x

    object suma extends Semigroup2Law

    def instanceMonoid[A](f: (A, A) => A, z: A): Monoid[A] = {
      new Monoid[A] {
        val identity = z
        def bin(f1: A, f2: => A): A = f(f1,f2)
      }
    }

    def instanceRing[A](f: (A, A) => A, g: (A, A) => A, z: A): Ring[A] = {
      new Ring[A] {
        val identity = z
        def inverse(x: A) =
        def bin(f1: A, f2: => A): A = f(f1,f2)
      }
    }

  }

  // val b = Z.suma.bin(3, 4)

}




