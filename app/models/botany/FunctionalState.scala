import scalaz._

// http://eed3si9n.com/learning-scalaz/sbt.html
object LearningScalazOK {


  def main(args: Array[String]): Unit = {

    import scalaz._

    println("Empezamos")

    type Stack = List[Int]

    val pop: State[Stack, Int] = State[Stack, Int] {
      case x :: xs => (xs, x)
    }

    def push(a: Int): State[Stack, Unit] = State[Stack, Unit] {
      case xs => (a :: xs, ())
    }

    def stackManip: State[Stack, Int] = for {
      _ <- push(6)
      _ <- push(7)
      cc <- push(8)
      ccc <- push(9)
      ccc <- push(10)
      a <- pop
      b <- pop
    } yield(b)

    println(stackManip(List()))

  }
}

object StateTest1 {

  def main(args: Array[String]): Unit = {



    import scalaz._

    type Stack = List[Int]

    val pop: State[Stack, Int] = State[Stack, Int] {
      case x :: xs => (xs, x)
    }

    def push(a: Int): State[Stack, Unit] = State[Stack, Unit] {
      case xs => (a :: xs, ())
    }

    def add(i: Int): State[Stack, Int] = for {
      _ <- push(i)
      _ <- push(i)
      b <- pop

    } yield b



    println("Empezamos")

    val s = State[Int, String](i => (i + 1, "valor = " + (i+1)))

    println(s.eval(11))


    println(s.exec(1))

    println(s(1))

    println("**********")

    val z1 = add(8)
    val z2 = add(9)
    val z3 = add(10)

    println(z1)


  }
}
