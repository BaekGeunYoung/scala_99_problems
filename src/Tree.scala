object TreeTest extends App {
  println(Tree.cBalanced(4, "x"))
}

sealed abstract class Tree[+T] {
  val height: Int
  val nodeCount: Int

  def isMirrorOf[V](other: Tree[V]): Boolean

  def isSymmetric: Boolean

  def addValue[U >: T](x: U)(implicit ev$1: U => Ordered[U]): Tree[U]

  def mirrored: Tree[T]

  def leafCount: Int

  def leafList: List[T]

  def internalList: List[T]

  def atLevel(level: Int): List[T]

  def atLevelOrder(levelOrder: Int): Tree[T]

  def replaceAtLevelOrder[U >: T](levelOrder: Int, Tree: Tree[U]): Tree[U]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override val height: Int = Math.max(left.height, right.height) + 1
  override val nodeCount: Int = left.nodeCount + right.nodeCount + 1

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

  override def leafCount: Int = (left, right) match {
    case (End, End) => 1
    case _ => left.leafCount + right.leafCount
  }

  override def leafList: List[T] = (left, right) match {
    case (End, End) => List(this.value)
    case _ => left.leafList ::: right.leafList
  }

  override def internalList: List[T] = (left, right) match {
    case (End, End) => Nil
    case _ => value :: left.internalList ::: right.internalList
  }

  override def atLevel(level: Int): List[T] = {
    if (level == 1) List(value)
    else left.atLevel(level - 1) ::: right.atLevel(level - 1)
  }

  override def atLevelOrder(levelOrder: Int): Tree[T] = {
    if (levelOrder == 1) return this

    atLevelOrder(levelOrder / 2) match {
      case n: Node[T] =>
        if (levelOrder % 2 == 0) n.left
        else n.right
      case End => End
    }
  }

  override def replaceAtLevelOrder[U >: T](levelOrder: Int, tree: Tree[U]): Tree[U] = {
    val parentLevelOrder = levelOrder / 2
    val parent = atLevelOrder(levelOrder / 2).asInstanceOf[Node[T]]

    if (parentLevelOrder == 1) {
      if (levelOrder % 2 == 0) Node(value, tree, parent.right)
      else Node(value, parent.left, tree)
    }
    else {
      if (levelOrder % 2 == 0) Node(value, tree, parent.right)
      else Node(value, parent.left, tree)
    }
  }
}

case object End extends Tree[Nothing] {
  override val height: Int = 0
  override val nodeCount: Int = 0

  override def toString = "."

  override def isMirrorOf[V](other: Tree[V]): Boolean = other == End

  override def isSymmetric: Boolean = true

  override def addValue[U >: Nothing](x: U)(implicit ev$1: U => Ordered[U]): Tree[U] = Node(x)

  override def mirrored: Tree[Nothing] = End

  override def leafCount: Int = 0

  override def leafList: List[Nothing] = Nil

  override def internalList: List[Nothing] = Nil

  override def atLevel(level: Int): List[Nothing] = Nil

  override def atLevelOrder(levelOrder: Int): Tree[Nothing] = End

  override def replaceAtLevelOrder[U >: Nothing](levelOrder: Int, tree: Tree[U]): Tree[U] = tree
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  val minHbalNodesCache: Array[Int] = new Array(Int.MaxValue)
  def cBalanced[A](n: Int, element: A): List[Tree[A]] = {
    if (n == 0) return List(End)
    if (n == 1) return List(Node(element))

    if (n % 2 == 0) {
      val subtreeSize1 = (n / 2) - 1
      val subtreeSize2 = (n / 2)

      val subtreeList1 = Tree.cBalanced(subtreeSize1, element)
      val subtreeList2 = Tree.cBalanced(subtreeSize2, element)

      subtreeList1.flatMap (e1 =>
        subtreeList2.flatMap (e2 =>
          List(Node(element, e1, e2), Node(element, e2, e1))
        )
      )
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

  def hbalTrees[A](h: Int, element: A): List[Tree[A]] = {
    if (h == 0) return List(End)
    if (h == 1) return List(Node(element))

    val gHeight = h - 1
    val lHeight = h - 2

    hbalTrees(gHeight, element).flatMap(gHbalTree =>
      hbalTrees(lHeight, element).flatMap(lHbalTree =>
        List(Node(element, gHbalTree, lHbalTree), Node(element, lHbalTree, gHbalTree))
      )
    )
  }

  def minHbalNodes(h: Int): Int = {
    if (h == 0) return 0
    if (h == 1) return 1

    if (minHbalNodesCache(h) != 0) return minHbalNodesCache(h)

    minHbalNodes(h - 1) + minHbalNodes(h - 2) + 1
  }

  def maxHbalNodes(height: Int): Int = 2 * height - 1

  def minHbalHeight(nodes: Int): Int =
    if (nodes == 0) 0
    else minHbalHeight(nodes / 2) + 1

  def maxHbalHeight(nodes: Int): Int = LazyList.from(1).takeWhile(minHbalNodes(_) <= nodes).last

  def hbalTreesWithNodes[T](nodes: Int, value: T): List[Tree[T]] =
    (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, value)).filter(_.nodeCount == nodes).toList

  def completeBinaryTree[T](nodes: Int, value: T): Tree[T] = {
    def generateTree(addr: Int): Tree[T] =
      if (addr > nodes) End
      else Node(value, generateTree(2 * addr), generateTree(2 * addr + 1))
    generateTree(1)
  }
}
