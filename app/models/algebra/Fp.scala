package models.algebra
import models.algebra.Utils._

case class Fp(p: Int) extends Field {
  require(isPrime(p), p + " is not a prime number")

  type T1 = Int
  type T2 = FpElement


  def builder(x: T1): T2 = FpElement(x)

  val identity = builder(1) // TODO conviven identity con one?
  val structureId: String = "Fp" + p.toString
  val finite: Boolean = true
  val zero = builder(0)

  override val one = builder(1)
  val minusOne = builder(p - 1)

  def modulo(x: T1): T1 = if (x < 0) -x else x

  private object FpElement {
    def apply(k: T1) = {
      val v: Int = if (k < 0) {
        (modulo(k) / p + 1) * p + k
      } else {
        k
      }
      new FpElement(v % p)
    }
  }
  class FpElement (val k: T1)  extends FieldElement {

    val elementId = k.toString
    val fatherFp = Fp.this
    val isZero = k == 0

    def add(other: FpElement) = builder((k + other.k) % p)
    def minus(other: FpElement) = builder((k - other.k) % p)
    def negate = builder(p - k)
    def multiply(other: FpElement) = builder((k * other.k) % p)

    /** Power of an element
      *
      * Uses repeated squaring algorithm:
      * http://www.algorithmist.com/index.php/Repeated_Squaring
      *
      * val cuerpo = Fp(43)
      * val a = cuerpo.builder(1520)
      * println(a.inverse*a)
      */
    def power(p: Int): FpElement = p match {
      case 0 => one
      case 1 => this
      case p if p % 2 == 1 => this * (this * this).power((p - 1) / 2)
      case p if p % 2 == 0 => (this * this).power(p / 2)
    }

    def inverse: T2 = {
      if (this == zero) {
        throw new IllegalArgumentException("zero does not have inverse")
      } else {
        this.power(p - 2)
      }
    }

    override def toString = k.toString // + " mod " + n.toString

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[T2]
      if (that == null) false
      else (this.k % p) == (that.k % p)
    }
  }
}