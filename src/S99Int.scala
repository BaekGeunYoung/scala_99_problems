object Test extends App {
  import S99Int._

  val a: Int = 315
  assert(a.primeFactors == List(3, 3, 5, 7))

  val b = 1009000

  val time1 = System.currentTimeMillis()
  b.totient
  val time2 = System.currentTimeMillis()
  b.totientImproved
  val time3 = System.currentTimeMillis()

  assert(time2 - time1 > time3 - time2)

  printGoldbach(9 to 20)

  printGoldbachListLimited(1 to 2000, 50)
}

class S99Int(val start: Int) {
  import S99Int._

  def isPrime: Boolean =
    (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })

  def gcd(a: Int, b: Int): Int = {
    val greater = if (a >= b) a else b
    val smaller = if (a >= b) b else a

    greater % smaller match {
      case 0 => smaller
      case r => gcd(smaller, r)
    }
  }

  def isCoprimeTo(other: S99Int): Boolean = gcd(this, other) == 1

  def totient: Int = (1 to start).count(e => start.isCoprimeTo(e))

  def primeFactors: List[Int] = {
    if (start == 1) return List()

    val firstPrimeDivisor = primes.find(prime => start % prime == 0).get
    firstPrimeDivisor :: (start / firstPrimeDivisor).primeFactors
  }

  def primeFactors2: List[Int] = {
    def primeFactorsR(n: Int, ps: Stream[Int]): List[Int] =
      if (n.isPrime) List(n)
      else if (n % ps.head == 0) ps.head :: primeFactorsR(n / ps.head, ps)
      else primeFactorsR(n, ps.tail)
    primeFactorsR(start, primes)
  }

  def primeFactorMultiplicity: List[(Int, Int)] = P10.encode(start.primeFactors).map(_.swap)

  def totientImproved: Int = start.primeFactorMultiplicity.foldLeft(1) { (acc, curr) =>
    val (p, m) = curr
    (acc * (p - 1) * Math.pow(p, m - 1)).toInt
  }

  def goldbach: (Int, Int) = {
    primes
      .takeWhile(p => p < start)
      .find(p => (start - p).isPrime) match {
      case None => throw new IllegalArgumentException
      case Some(prime) => (prime, start - prime)
    }
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  implicit def s99Int2Int(i: S99Int): Int = i.start

  val primes: Stream[Int] = Stream.cons(2, Stream.from(3, 2) filter(e => e.isPrime))

  def printGoldbach(range: Range): Unit = {
    range.filter(_ % 2 == 0).foreach(p => println(s"$p = ${p.goldbach._1} + ${p.goldbach._2}"))
  }

  def printGoldbachListLimited(range: Range, min: Int): Unit = {
    range
      .filter(e => e % 2 == 0 && e > 2)
      .foreach { e =>
        val (left, right) = e.goldbach
        if (left > min && right > min) println(s"$e = $left + $right")
      }
  }
}
