package sorting

object Sort {
	
  def bubble(xs: List[Int]): List[Int] = {
		def inner(ys: List[Int], i: Int, switched: Boolean): List[Int] = {
			if (ys.size-1 == i) {
				if (switched) inner(ys, 0, false)	else ys
			}	else if (ys(i) > ys(i+1)) {
				val zs = ys.take(i) ::: List(ys(i+1)) ::: List(ys(i)) ::: ys.drop(i+2)
				inner(zs, i+1, true)
			} else inner(ys, i+1, switched)
		}
		
		inner(xs, 0, false)
	}                                         

}