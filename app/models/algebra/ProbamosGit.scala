// Prueba3
package models.algebra

object ProbamosGit {

  // All factors of a number (not only primes)
  def factors(num: Int) = {
    (1 to num).filter { divisor =>
      num % divisor == 0
    }
  }

  // Least prime divisor of n greater or iqual to k
  // TODO Esto no funciona ldf(48,4) dice 4
  def ldf(n: Int, k: Int): Int = {
    if (n % k == 0) {
      k
    } else {
      if (k * k > n) {
        n
      } else {
        ldf(k + 1, n)
      }
    }
  }


  // Least prime divisor of n
  def ld (n: Int): Int = ldf(n, 2)

  def prime0(n: Int) = n match {
    case 1 => false
    case x => ld(x) == x
  }

  // Power of integers TODO Usar la técnica del Cohen de asociar
  def expInt(n:Int, m: Int): Int = List.fill(m)(n).product


  def primeFactors(number: Int): List[(Int, Int)] = {

    // all prime factors with repetition
    def factorize(x: Int): List[Int] = {
      def loop(x: Int, a: Int): List[Int] = if (a * a > x) List(x) else x % a match {
        case 0 => a :: loop(x / a, a)
        case _ => loop(x, a + 1)
      }
      loop(x, 2)
    }

    val fact = factorize(number)
    val uniq = fact.toSet.toList
    val emptyList: List[(Int, Int)] = List[(Int, Int)]()
    val unflattened = for (i <- uniq) yield (i, fact.count(_ == i)) :: emptyList
    unflattened.flatten
  }
}

