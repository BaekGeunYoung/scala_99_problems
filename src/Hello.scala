import java.util.NoSuchElementException

object Hello extends App {
  val list = List(1, 2, 3, 4, 5)

  assert(P01.last(list) == 5)
  assert(P02.lastButOne(list) == 4)
  assert(P03.nth(3, list) == list(3))
  assert(P04.length(list) == list.length)
  assert(P05.reverse(list) == list.reverse)
  assert(P05.reverseTailRec(list) == list.reverse)
  assert(P07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  assert(P08.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  assert(P08.compressFunctional(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  assert(
    P09.pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) ==
    List(
      List('a', 'a', 'a', 'a'),
      List('b'),
      List('c', 'c'),
      List('a', 'a'),
      List('d'),
      List('e', 'e', 'e', 'e'),
    )
  )
}

object P01 {
  @scala.annotation.tailrec
  def last[A](list: List[A]): A =
    list match {
      case h :: Nil => h
      case _ :: tail => last(tail)
      case _ => throw new NoSuchElementException
    }
}

object P02 {
  @scala.annotation.tailrec
  def lastButOne[A](list: List[A]): A =
    list match {
      case _ :: Nil => throw new NoSuchElementException
      case h1 :: tail => tail match {
        case _ :: Nil => h1
        case _ => lastButOne(tail)
      }
      case _ => throw new NoSuchElementException
    }

  // better solution
  @scala.annotation.tailrec
  def lastButOne2[A](list: List[A]): A = list match {
    case h :: _ :: Nil => h
    case _ :: tail => lastButOne2(tail)
    case _ => throw new NoSuchElementException
  }
}

object P03 {
  @scala.annotation.tailrec
  def nth[A](n: Int, list: List[A]): A = list match {
    case h :: tail => if (n == 0) h else nth(n - 1, tail)
    case _ => throw new NoSuchElementException
  }

  // better solution
  @scala.annotation.tailrec
  def nth2[A](n: Int, list: List[A]): A = (n, list) match {
    case (0, h :: _) => h
    case (_, _ :: tail) => nth2(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }
}

object P04 {
  // simple recursive
  def length[A](list: List[A]): Int = list match {
    case _ :: tail => 1 + length(tail)
    case _ => 0
  }

  // make tail recursive
  def lengthTailRecursive[A](list: List[A]): Int =  {
    @scala.annotation.tailrec
    def lengthR(result: Int, curList: List[A]): Int = curList match {
      case Nil => result
      case _ :: tail => lengthR(result + 1, tail)
    }

    lengthR(0, list)
  }
}

object P05 {
  // simple recursive
  def reverse[A](list: List[A]): List[A] = list match {
    case h :: tail => reverse(tail) ::: List(h)
    case _ => Nil
  }

  // tail recursive
  def reverseTailRec[A](list: List[A]): List[A] = {
    @scala.annotation.tailrec
    def reverseAppend(result: List[A], curList: List[A]): List[A] = curList match {
      case h :: tail => reverseAppend(h :: result, tail)
      case _ => result
    }

    reverseAppend(List(), list)
  }
}

object P06 {
  def isPalindrome[A](list: List[A]): Boolean = list == list.reverse
}

object P07 {
  def flatten(list: List[Any]): List[Any] = list match {
    case h :: tail => h match {
      case list: List[Any] => flatten(list) ::: flatten(tail)
      case notList => List(notList) ::: flatten(tail)
    }
    case _ => Nil
  }

  // using flatMap
  def flatten2(list: List[Any]): List[Any] = list.flatMap {
    case ls: List[_] => flatten2(ls)
    case e => List(e)
  }
}

object P08 {
  def compress(list: List[Symbol]): List[Symbol] = {
    @scala.annotation.tailrec
    def compressFold(curSymbol: Symbol, curList: List[Symbol], result: List[Symbol]): List[Symbol] = curList match {
      case h :: tail =>
        if (curSymbol == h) compressFold(curSymbol, tail, result)
        else compressFold(h, tail, result ::: List(h))
      case _ => result
    }

    list match {
      case h :: tail => compressFold(h, tail, List(h))
      case _ => Nil
    }
  }

  // using dropWhile
  def compressTailRecursive[A](ls: List[A]): List[A] = {
    @scala.annotation.tailrec
    def compressR(result: List[A], curList: List[A]): List[A] = curList match {
      case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
      case Nil       => result.reverse
    }

    compressR(Nil, ls)
  }

  // using fold
  def compressFunctional[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]()) { (h, r) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
    }
}

object P09 {
  def pack[A](ls: List[A]): List[List[A]] = {
    @scala.annotation.tailrec
    def packR(result: List[List[A]], curList: List[A]): List[List[A]] = curList match {
      case h :: tail => packR((h :: tail.takeWhile(_ == h)) :: result, tail.dropWhile(_ == h))
      case Nil => result.reverse
    }

    packR(List(), ls)
  }

  // using span
  def pack2[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
}

object P10 {
  def encode[A](list: List[A]): List[(Int, A)] = P09.pack(list).map(e => (e.length, e.head))
}

object P11 {
  def encodeModified[A](list: List[A]): List[Any] = P09.pack(list).map { e =>
    if (e.length == 1) e.head
    else (e.length, e.head)
  }
}

object P12 {
  def decode[A](ls: List[(Int, A)]): List[A] = ls.flatMap(e => List.fill(e._1)(e._2))
}