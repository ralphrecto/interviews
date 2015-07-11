import scala.util.control.Breaks._

object Sorting {
  // For simplicity we will be doing ascending sorts

  // O(n^2) sorts

  // Idea: keep a "sorted" and "unsorted" section, repeatedly
  // pull elements from unsorted and put them into sorted
  def selectionSort(input: Array[Int]): Array[Int] = {
    // Invariant: [0, start] is sorted
    for (start <- 0 until input.length) {
      val temp = input(start)
      var min : Option[(Int, Int)] = None
      // Find min in unsorted section
      for (i <- start until input.length) {
        min match {
          case None => min = Some(input(i), i)
          case Some((x, _)) => if (x > input(i)) min = Some(input(i), i)
        }
      }
      // Switch min and value at the end of sorted section
      min match {
        case None => () // erronous case...
        case Some((value, pos)) => {
          input(start) = value
          input(pos) = temp
        }
      }
    }
    input
  }

  // Note that in one pass, bubbleSort places the greatest element in the
  // last position in the array. In an array of size n, if the greatest
  // element x is in index i for i < n-1, then in the first pass, x will
  // be swapped with each elements in the indices [i+1, n-1]. This is
  // generalized by saying that in the nth pass, the nth greatest element
  // is put into its sorted place. The invariant here is similar to selection
  // sort: there is a "sorted" section (the end of the array with the greatest
  // elements) and an unsorted section.
  def bubbleSort(input: Array[Int]) = {
    breakable {
      var noSwaps = true
      for (end <- (0 until (input.length - 1)).reverse) {
        for (i <- 0 until end) {
          if (input(i) > input(i+1)) {
            // swap places in the array (without a temp!)
            input(i) += input(i+1)
            input(i+1) = input(i) - input(i+1)
            input(i) -= input(i+1)
            noSwaps = false
          }
        }
        if (noSwaps) break else noSwaps = false
      }
    }
    input
  }

  // Again, same idea as the previous two: keep a sorted section
  // and an unsorted section.
  def insertionSort(input: Array[Int]) = {

  }

  def main(args: Array[String]) = {
    val test1 = Array(5, 7, 3, 4, 2, -100)
    util.Util.printArray(selectionSort(test1))
    util.Util.printArray(bubbleSort(test1))
  }
}
