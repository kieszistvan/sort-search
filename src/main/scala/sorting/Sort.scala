package sorting

object Sort {
	
  def bubble(xs: List[Int]): List[Int] = {
	if (xs.isEmpty) return xs
	
    def inner(ys: List[Int], i: Int, switched: Boolean): List[Int] = {
		if (ys.size-1 == i) {
			if (switched) inner(ys, 0, false) else ys
		}	else if (ys(i) > ys(i+1)) {
			val zs = ys.take(i) ::: List(ys(i+1)) ::: List(ys(i)) ::: ys.drop(i+2)
			inner(zs, i+1, true) 
		} else inner(ys, i+1, switched)
	}
	
	inner(xs, 0, false)
  }                                         

  def isSorted(xs: List[Int]):Boolean = {
    if (xs.isEmpty) return true
    
    try {
      xs.fold(xs.head)((a, b) => if (a <= b) b else throw new SortingException)
      return true
    } catch {
      case e: SortingException => return false
      case _:Throwable => throw new RuntimeException
    }
  }
  
}

class SortingException extends RuntimeException