package sorting

import scala.math.Ordered

object Sort {

  def emptyOrSingleChecked[A <% Ordered[A]](xs: List[A])(f: List[A] => List[A]): List[A] = if (xs.length <= 1) xs else f(xs)

  def bubble[A <% Ordered[A]](xs: List[A]): List[A] = emptyOrSingleChecked(xs) { xs =>

    def inner(ys: List[A], i: Int, switched: Boolean): List[A] = {
      if (ys.size - 1 == i) {
        if (switched) inner(ys, 0, false) else ys
      } else if (ys(i) > ys(i + 1)) {
        val zs = ys.take(i) ::: List(ys(i + 1)) ::: List(ys(i)) ::: ys.drop(i + 2)
        inner(zs, i + 1, true)
      } else inner(ys, i + 1, switched)
    }

    inner(xs, 0, false)
  }

  def isSorted[A <% Ordered[A]](xs: List[A]): Boolean = {
    if (xs.length <= 1) return true

    try {
      xs.fold(xs.head)((a, b) => if (a <= b) b else throw new SortingException)
      true
    } catch {
      case e: SortingException => return false
      case _: Throwable => throw new RuntimeException
    }
  }

  def insertion[A <% Ordered[A]](xs: List[A]): List[A] = emptyOrSingleChecked(xs) {
    xs =>

      def inner(sorted: List[A], unsorted: List[A]): List[A] = {
        if (unsorted.isEmpty) return sorted
        inner(put(unsorted.head, sorted), unsorted.tail)
      }

      // put the element into a sorted list
      def put(elem: A, ys: List[A]): List[A] = {
        ys match {
          case h :: t =>
            if (elem > h) h :: put(elem, t)
            else elem :: ys
          case Nil => ys ::: List(elem)
        }
      }
      inner(Nil, xs)
  }

  // as in Programmin in Scala
  def merge[A <% Ordered[A]](xs: List[A]): List[A] = emptyOrSingleChecked(xs) {
    xs =>
      def inner(ys: List[A], zs: List[A], acc: List[A]): List[A] = {

        (ys, zs) match {
          // these cases cover the one-element situation which is higher than the one an iteration earlier
          case (Nil, _) => zs.reverse ::: acc
          case (_, Nil) => ys.reverse ::: acc
          case (yh :: yt, zh :: zt) =>
            if (yh < zh) inner(yt, zs, yh :: acc)
            else inner(ys, zt, zh :: acc)
        }

      }

      val n = xs.length / 2
      if (n == 0) xs
      else {
        val (ys, zs) = xs splitAt n
        inner(merge(ys), merge(zs), Nil).reverse // split until there are only pairs (x,y or x,Nil)
      }
  }

  def quick[A <% Ordered[A]](xs: List[A]): List[A] = emptyOrSingleChecked(xs) {
    xs =>

      val pivot = xs(xs.length / 2)

      val splitted = xs.foldLeft((List[A](), List[A](), List[A]())) {
        (x, i) =>
          if (i < pivot) (((i :: x._1), x._2, x._3))
          else if (i == pivot) ((x._1, (i :: x._2), x._3))
          else ((x._1, x._2, (i :: x._3)))
      }

      quick(splitted._1) ::: splitted._2 ::: quick(splitted._3)
  }

}

class SortingException extends RuntimeException