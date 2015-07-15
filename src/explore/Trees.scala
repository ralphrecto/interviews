package explore

import util._

object Trees {

  def treeHeight[T](tree: Tree[T], f: (Int, Int) => Int): Int = tree match {
    case Leaf(_) => 0
    case Node(l, _, r) => {
      f(treeHeight(l, f), treeHeight(r, f)) + 1
    }
  }

  def minHeight[T](tree: Tree[T]) = treeHeight(tree, Math.min)

  def maxHeight[T](tree: Tree[T]) = treeHeight(tree, Math.max)

  // precond: tree is a BST
  // Idea: formalize "is balanced" to be equivalent to the statement abs(minHeight(t) - maxHeight(t)) <= 1.
  // Clearly if a tree t is balanced then its children are also. Suppose t.left and t.right are both balanced.
  // If t.left and t.right have the same max height, then they also have the same min height, so t is balanced.
  // If their max heights differ by more than 1 then they cannot possibly be balanced. Finally, consider the
  // case when their max heights differ by exactly 1; suppose WLOG maxHeight(t.left) == n and maxHeight(t.right)
  // == n+1. In the worst case, minHeight(t.left) == n-1 since t.left is balanced. But this implies that t cannot
  // be balanced, hence we return false in this case.
  def isBalanced[T](tree: Tree[T]): Boolean = tree match {
    case Leaf(_) => true
    case Node(l, v, r) => {
      val lMaxHeight = maxHeight(l)
      val rMaxHeight = maxHeight(r)
      if (isBalanced(l) && isBalanced(r)) {
        if (lMaxHeight == rMaxHeight) true
        else if (Math.abs(lMaxHeight - rMaxHeight) == 1) {
          Math.abs(minHeight(l) - rMaxHeight) <= 1 &&
            Math.abs(minHeight(r) - lMaxHeight) <= 1
        } else false
      } else false
    }
  }

}
