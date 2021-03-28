object LogicTest extends App {
  import S99Logic._

  println(gray(3))
}

object S99Logic {
  implicit def boolean2S99Logic(b: Boolean): S99Logic = new S99Logic(b)

  def not(a: Boolean): Boolean = if (a) false else true

  def gray(n: Int): List[String] =
    if (n == 0) List("")
    else {
      val a = gray(n - 1)
      List("0", "1")
        .flatMap(s => a.map(e => s ++ e))
    }
}

class S99Logic(a: Boolean) {
  import S99Logic._

  def and(b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _            => false
  }
  def or(b: Boolean): Boolean = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _         => false
  }
  def equ(b: Boolean): Boolean = (a and b) or (not(a) and not(b))
  def xor(b: Boolean): Boolean = not(a equ b)
  def nor(b: Boolean): Boolean = not(a or b)
  def nand(b: Boolean): Boolean = not(a and b)
  def impl(b: Boolean): Boolean = not(a) or b
}
