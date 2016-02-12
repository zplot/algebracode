package models.algebra

case object Z extends UFD {

  type T1 = Int
  type T2 = ZInteger



  def builder(x: T1) = ZInteger(x)

  val structureId: String = "Z"
  val finite = false
  val zero = builder(0)
  val one = builder(1)


  def modulo(x: T1): T1 = if (x < 0) -x else x

  def normalPart(x: T2): T2 = {
    if (x.k > 0) x else x.negate
  }
  def euclideanNorm(x: T2): BigInt = modulo(x.k)
  def euclideanQ(a: T2, b: T2): T2 =
    if (b == zero) {
      throw new IllegalArgumentException("dividing by zero...")
    } else {
      builder(a.k / b.k)
    }

  def euclideanR(a: T2, b: T2): T2 = builder(a.k % b.k)

  def gcd(a: T2, b: T2): T2 =
    if (a == zero && b == zero) zero else {
      if (b == zero) a else gcd(b, euclideanR(a, b))
    }

  // TODO
  def bezout(a: T2, b: T2): (T2, T2) = (a,b)



  object ZInteger {
    def apply(k: T1): ZInteger = {
      new ZInteger(k)
    }
  }
  class ZInteger private(val k: T1) extends UFDElement {

    val elementId = k.toString
    val isZero = k == 0

    def add(other: ZInteger) = ZInteger(k + other.k)
    def minus(other: ZInteger) = ZInteger(k - other.k)
    def negate: T2 = ZInteger(-k)
    def multiply(other: ZInteger) = ZInteger(k * other.k)
    override def toString = k.toString
    def minus = ZInteger(-this.k)


    final override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[ZInteger]
      if (that == null) false
      else this.k == that.k
    }
  }
}
