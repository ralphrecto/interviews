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
  // Idea: formalize "is balanced" to be equivalent to the statement
  // abs(minHeight(tree)-maxHeight(tree)) <= 1
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
