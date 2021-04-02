object TreeTest extends App {
  println(Tree.cBalanced(4, "x"))
}

sealed abstract class Tree[+T] {
  def isMirrorOf[V](other: Tree[V]): Boolean

  def isSymmetric: Boolean

  def addValue[U >: T](x: U)(implicit ev$1: U => Ordered[U]): Tree[U]

  def mirrored: Tree[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = s"T(${value.toString} ${left.toString} ${right.toString})"

  override def isMirrorOf[V](other: Tree[V]): Boolean = other match {
    case n: Node[V] => this.left.isMirrorOf(n.right) && this.right.isMirrorOf(n.left)
    case _ => false
  }

  override def isSymmetric: Boolean = left.isMirrorOf(right)

  override def addValue[U >: T](x: U)(implicit ev$1: U => Ordered[U]): Tree[U] =
    if (x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))

  override def mirrored: Tree[T] = Node(value, right.mirrored, left.mirrored)
}

case object End extends Tree[Nothing] {
  override def toString = "."

  override def isMirrorOf[V](other: Tree[V]): Boolean = other == End

  override def isSymmetric: Boolean = true

  override def addValue[U >: Nothing](x: U)(implicit ev$1: U => Ordered[U]): Tree[U] = Node(x)

  override def mirrored: Tree[Nothing] = End
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def cBalanced[A](n: Int, element: A): List[Tree[A]] = {
    if (n == 0) return List(End)
    if (n == 1) return List(Node(element))

    if (n % 2 == 0) {
      val subtreeSize1 = (n / 2) - 1
      val subtreeSize2 = (n / 2)

      val subtreeList1 = Tree.cBalanced(subtreeSize1, element)
      val subtreeList2 = Tree.cBalanced(subtreeSize2, element)

      val case1 = subtreeList1.flatMap (e1 =>
        subtreeList2.map (e2 =>
          Node(element, e1, e2)
        )
      )

      val case2 = subtreeList2.flatMap (e1 =>
        subtreeList1.map (e2 =>
          Node(element, e1, e2)
        )
      )

      case1 ::: case2
    }
    else {
      val subtreeSize = n / 2

      val subtreeList = Tree.cBalanced(subtreeSize, element)

      subtreeList.flatMap { e1 =>
        subtreeList.map { e2 =>
          Node(element, e1, e2)
        }
      }
    }
  }

  def fromList[A](ls: List[A])(implicit ev$1: A => Ordered[A]): Tree[A] = {
    @scala.annotation.tailrec
    def fromListR(result: Tree[A], curList: List[A]): Tree[A] = curList match {
      case h :: tail => fromListR(result.addValue(h), tail)
      case _ => result
    }

    fromListR(End, ls)
  }

  def symmetricBalancedTrees[A](n: Int, a: A): List[Tree[A]] = {
    if (n % 2 == 0) List()
    else {
      val subtreeSize = n / 2

      cBalanced(subtreeSize, a).map (subtree => Node(a, subtree, subtree.mirrored))
    }
  }
}