package sorting

object Sort {

  def emptyChecked(xs: List[Int])(f: List[Int] => List[Int]): List[Int] = if (xs.isEmpty) List() else f(xs)

  def bubble(xs: List[Int]): List[Int] = emptyChecked(xs) { xs =>

    def inner(ys: List[Int], i: Int, switched: Boolean): List[Int] = {
      if (ys.size - 1 == i) {
        if (switched) inner(ys, 0, false) else ys
      } else if (ys(i) > ys(i + 1)) {
        val zs = ys.take(i) ::: List(ys(i + 1)) ::: List(ys(i)) ::: ys.drop(i + 2)
        inner(zs, i + 1, true)
      } else inner(ys, i + 1, switched)
    }

    inner(xs, 0, false)
  }

  def isSorted(xs: List[Int]): Boolean = {
    if (xs.isEmpty) return true

    try {
      xs.fold(xs.head)((a, b) => if (a <= b) b else throw new SortingException)
      true
    } catch {
      case e: SortingException => return false
      case _: Throwable => throw new RuntimeException
    }
  }

  def insertion(xs: List[Int]): List[Int] = emptyChecked(xs) {
    xs =>

      def inner(sorted: List[Int], unsorted: List[Int]): List[Int] = {
        if (unsorted.isEmpty) return sorted
        inner(put(unsorted.head, sorted), unsorted.tail)
      }

      // put the element into a sorted list
      def put(elem: Int, ys: List[Int]): List[Int] = {
        ys match {
          case h :: t =>
            if (elem > h) h :: put(elem, t)
            else elem :: ys
          case Nil => ys ::: List(elem)
        }
      }
      inner(Nil, xs)
  }

}

class SortingException extends RuntimeException