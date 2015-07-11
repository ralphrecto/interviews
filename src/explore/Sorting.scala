

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

  def printArray[T](arg: Array[T]) = {
    println(arg.toList)
  }

  def main(args: Array[String]) = {
    printArray(selectionSort(Array(5, 7, 3, 4, 2, -100)))
  }
}
