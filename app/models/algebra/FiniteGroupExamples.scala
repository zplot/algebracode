package models.algebra

import scala.util.matching.Regex

object FiniteGroupExamples {

  /* Cyclic group of n elements */
  def C(n: Int): PermutationGroup = {
    if (n < 1) println("n es menor que 2: no vale")
    val g1 = Permutation(Set(Cycle((1 to n).toList)))
    PermutationGroup(Set(g1), name = "Z" + n)
  }

  /* Symetric group of n elements */
  def S(n: Int): PermutationGroup = {
    if (n < 2) println("n es menor que 2: no vale")
    val g1 = Permutation(Set(Cycle((1 to n).toList)))
    val g2 = Permutation(Set(Cycle(List(1, 2))))
    PermutationGroup(Set(g1, g2), name = "S" + n)
  }

  /* Auxiliar para generar grupos alternados */
  def is3Cycle(g: Permutation): Boolean = {
    g.lco.length == 1 && g.lco.head.length == 3
  }

  /* Alternate group of n elements */
  def A(n: Int): PermutationGroup = {
    if (n < 3) println("n es menor que 3: no vale")
    val tmp0 = S(n)
    val tmp1 = tmp0.permutationSet
    val tmp11 = tmp1.filter(x => is3Cycle(x))
    PermutationGroup(tmp11, name = "A" + n)
  }

  /* Quaternions */
  def Q8: PermutationGroup = {
    val p1 = Permutation(Set(Cycle(List(1, 5, 2, 6)), Cycle(List(3, 8, 4, 7))))
    val p2 = Permutation(Set(Cycle(List(1, 3, 2, 4)), Cycle(List(5, 7, 6, 8))))
    val p3 = Permutation(Set(Cycle(List(1, 2)), Cycle(List(3, 4)), Cycle(List(5, 6)), Cycle(List(7, 8))))
    PermutationGroup(Set(p1, p2, p3), name = "Q8")
  }

  /* Dihedral groups */

  case class D(n: Int) extends FiniteGroup {

    type T1 = (Int, Int)
    type T2 = DnElement

    def builder(x: T1): T2 = DnElement(x._1, x._2)

    val structureId = "D" + n
    val tmp1: IndexedSeq[T2] = for (i <- 0 to n - 1; j <- 0 to 1) yield DnElement(i, j)
    val elements: Set[T2] = tmp1.toSet
    val identity = DnElement(0, 0)
    val one = identity

    override def isAbelian = if (n > 2) false else true

    override def cardinal = 2 * n

    object DnElement {
      def apply(rot: Int, flip: Int): DnElement = {
        new DnElement(rot % n, flip % 2) // rot son las rotaciones, flip es la inversión (flip)
      }
    }

    class DnElement(val rot: Int, val flip: Int) extends FiniteGroupElement {

      override val elementId = "(" + rot + "," + flip + ")"

      def inverse: T2 = {
        if (this.flip == 0) {
          new DnElement(n - this.rot, 0)
        } else {
          new DnElement(this.rot, 1) // TODO Comprobar esto
        }
      }

      def multiply(other: DnElement): T2 = {
        if (this.flip == 0) {
          new DnElement((this.rot + other.rot) % n, other.flip)
        } else {
          new DnElement((this.rot - other.rot + n) % n,
            (other.flip + 1) % 2)
        }
      }

      final override def equals(other: Any): Boolean = {
        val that = other.asInstanceOf[DnElement]
        if (that == null) false
        else this.rot == that.rot && this.flip == that.flip
      }

      final override def hashCode = 13 * rot.hashCode() + 17 * flip.hashCode()

      override def toString = "ρ" + rot + "τ" + flip

    }

  }

  /* Direct Product of Groups */

  case class DirectProduct(group1: FiniteGroup, group2: FiniteGroup) extends FiniteGroup {

    type T1 = (group1.T2, group2.T2)
    type T2 = DirectProductElement



    def builder(par: T1): DirectProductElement = DirectProductElement(par)

    val structureId = group1.structureId + " x " + group2.structureId
    val identity = DirectProductElement(group1.one, group2.one)
    val one = identity

    val elements = {
      val tmp1: Set[T2] = for (i <- group1.elements; j <- group2.elements) yield DirectProductElement(i, j)
      tmp1.toSet
    }

    override def isAbelian = group1.isAbelian && group2.isAbelian

    override def cardinal = group1.cardinal * group2.cardinal


    case class DirectProductElement(par: T1) extends FiniteGroupElement {

      val elementId = "(" + par._1.elementId + ", " + par._2.elementId + ")"

      def inverse: T2 = builder(par._1.inverse, par._2.inverse)

      def multiply(other: DirectProductElement): T2 = {
        val tmp1 = this.par._1.multiply(other.par._1)
        val tmp2 = this.par._2.multiply(other.par._2)
        DirectProductElement(tmp1, tmp2)
      }

      final override def equals(other: Any): Boolean = {
        val that = other.asInstanceOf[DirectProductElement]
        if (that == null) false
        else this.par._1 == that.par._1 && this.par._2 == that.par._2
      }

      final override def hashCode = 13 * par._1.hashCode() + 17 * par._2.hashCode()

      override def toString = elementId


    }

  }



  def fromStringToGroup(s: String): Either[String, FiniteGroup] = {

    def DirectProductEither(group1: Either[String, FiniteGroup], group2: Either[String, FiniteGroup]): Either[String, FiniteGroup] = {
      val pair = (group1, group2)
      val result = pair match {
        case (Right(x), Right(y)) => Right(DirectProduct(x, y))
        case _ => Left("The groups for the direct product are not properly written. Please review admitted groups format")
      }
      result
    }

    val PatternS = """S\((\d)\)""".r
    val PatternC = """C\((\d)\)""".r
    val PatternA = """A\((\d)\)""".r
    val PatternD = """D\((\d)\)""".r
    val PatternDirectProduct = """DirectProduct\((.*?), (.*?)\)""".r

    s match {

      case PatternS(n) => Right(S(n.toInt))
      case PatternC(n) => Right(C(n.toInt))
      case PatternA(n) => Right(A(n.toInt))
      case PatternD(n) => Right(D(n.toInt))
      case "Q8" => Right(Q8)
      case PatternDirectProduct(x,y) => {
        println("x = " + x)
        println("y = " + y)
        val tmp = DirectProductEither(fromStringToGroup(x), fromStringToGroup(y))
        println(tmp)
        tmp
      }
      case _ => Left("The group is not properly written. Please review admitted groups format")

    }
  }
}


