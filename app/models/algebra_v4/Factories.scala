package models.algebra_v4

object Factories {
  def instanceMagma[A](f: (A, A) => A, z: A): Magma[A] = {
    new Magma[A] {
      val one = z
      def bin(f1: A, f2: A): A = f(f1, f2)
    }
  }

}