import scalaz._

// http://eed3si9n.com/learning-scalaz/sbt.html
object LearningScalazOK {


  def main(args: Array[String]): Unit = {

    import scalaz._

    println("Empezamos")

    type Stack = List[Int]

    val pop = State[Stack, Int] {
      case x :: xs => (xs, x)
    }

    def push(a: Int) = State[Stack, Unit] {
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
