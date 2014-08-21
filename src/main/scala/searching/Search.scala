package searching

import java.lang.RuntimeException

object Search {

  def emptyChecked[A <% Ordered[A]](xs: List[A])(f: List[A] => Int) = if (xs.isEmpty) -1 else f(xs)

  def binary[A <% Ordered[A]](xs: List[A], t: A): Int = emptyChecked(xs) {
    xs =>

      def inner(ys: List[A], l: Int, u: Int): Int = {
        if (l > u) return -1

        val half = l + (u - l + 1) / 2
        println("half " + half)
        ys match {
          case yh :: yt if (ys(half) == t) => half
          case yh :: yt if (ys(half) < t) => inner(ys, half + 1, u)
          case yh :: yt if (ys(half) > t) => inner(ys, l, half - 1)
        }
      }
      inner(xs, 0, xs.length - 1)
  }

}