package problems

/* How many binary trees are there with n nodes?
 *
 * We can derive the formula recursively; suppose f(n) denotes
 * the number of binary trees with n nodes. Clearly f(0) = 1.
 * For n > 1, we have precisely 1 node as the root, so there
 * are n-1 nodes in either the left or right subtree. If there
 * are k nodes in the left, then there necessarily are n-1-k
 * nodes on the right subtree. For a fixed k, there are
 * f(k) configurations for the left subtree and f(n-1-k)
 * configurations of the right; the choice of the left
 * and right configs are independent, so there are f(k)f(n-1-k)
 * binary trees of size n for a fixed k. Each k is disjoint
 * so we can simply sum for all possible values of k, which
 * is precisely the integral values in [0,n-1]. In sum, we
 * have the following recurrence:
 *
 * f(n) = \sum_{k=0}^{n-1}f(k)f(n-1-k)
 * f(0) = 1
 */
object Problem004 {
  def f(n: Int): Int = {
    if (n == 0) { return 1 }
    else (0 to n-1).foldLeft(0)((acc, k) => acc + (f(k)*f(n-1-k)))
  }

  def main(args: Array[String]): Unit ={
    println(f(3))
  }
}
