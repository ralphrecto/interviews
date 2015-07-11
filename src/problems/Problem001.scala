package problems

/* From: http://courses.csail.mit.edu/iap/interview/Hacking_a_Google_Interview_Handout_2.pdf
 * A Person is represented by a pair of (String, Int), their name and a position on a number line.
 * Given an array of Persons and a natural k, return a Map[Person, List[Person]) where each person
 * is mapped to their nearest k neighbors.
 *
 * Strategy: sort persons using their number as the key. Then, consider a window of size 2k around
 * each person (not including themselves), having the window extend k elements before and after
 * the person. The k closest neighbors is guaranteed to be in this window of size 2k. Suppose person
 * x is not in this window for person y and x is one of the closest k neighbors of y. Since x is
 * not in the window, there are at least k elements in order between x and y, which is absurd
 * since this implies that there are at least k elements closer to y than x.
 */
object Problem001 {

  case class Person(name: String, number: Int) {
    override def toString(): String = {
      s"(${this.name}, ${number})"
    }
  }

  def merge[T](left: Iterable[T], right: Iterable[T], k: Int, proj: T => Int): List[T] = {
    def inner(acc: List[T], left2: List[T], right2: List[T]): List[T] = {
      if (acc.size == k) acc
      else {
        (left2, right2) match {
          case (List(), List()) => acc
          case (leftHd :: leftTl, List()) => inner(leftHd :: acc, leftTl, right2)
          case (List(), rightHd :: rightTl) => inner(rightHd :: acc, left2, rightTl)
          case (leftHd :: leftTl, rightHd :: rightTl) => {
            if (proj(leftHd) < proj(rightHd)) inner(leftHd :: acc, leftTl, right2)
            else inner(rightHd :: acc, left2, rightTl)
          }
        }
      }
    }
    inner(List[T](), left.toList, right.toList)
  }

  def nearestKNeighbors(input: List[Person], k: Int) = {
    val sorted = input.sortBy(_.number)
    (0 until sorted.length).map(i => {
      val left = (Math.max(i-k, 0) until i).reverse.map(sorted(_))
      val right = (i+1 until Math.min(i+k, sorted.length)).map(sorted(_))
      (sorted(i), merge[Person](left, right, k, p => Math.abs(p.number - sorted(i).number)))
    }).toMap
  }

  def main(args: Array[String]) = {
    println(nearestKNeighbors(
      List(Person("bob", 10), Person("alice", 5), Person("jane", 7),
        Person("dude", 17), Person("dude2", 49), Person("guy", 43),
        Person("man1", 13), Person("woman2", 8)), 3))
  }

}
