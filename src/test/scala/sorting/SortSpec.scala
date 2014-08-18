package sorting

import org.specs2.mutable._

class SortSuite extends Specification {
	import Sort._

	"Sort#bubble" should {
	  "sort empty list without failure" in {
	    val empty = List[Int]()
	    bubble(empty) must equalTo(List())
	  } 
	  "sort unsorted list" in {
	    val unsorted = List(4,9,1,3,8)
	    bubble(unsorted) must equalTo(List(1,3,4,8,9))
	  }
	  "sort list with negative integers" in {
	    val unsortedWithNegative = List(9,8,-3,2,-9)
	    bubble(unsortedWithNegative) must equalTo(List(-9, -3, 2, 8 ,9))
	  }
	  "sort list with repeating integers" in {
	    val unsortedWithRepeatingInts = List(0,1,0,1,0,1,2,2,-1)
	    bubble(unsortedWithRepeatingInts) must equalTo(List(-1,0,0,0,1,1,1,2,2))
	  }
	}
	
	"Sort#isSorted" should {
	  "consider empty list sorted" in {
	    val empty = List()
	    isSorted(empty) must equalTo(true)
	  }
	  "recognize correctly sorted list" in {
	    val sorted = List(1,2,3,9,12)
	    isSorted(sorted) must equalTo(true)
	  }
	  "recognize if list is not sorted" in {
	    val unsorted = List(1,3,2,0)
	    isSorted(unsorted) must equalTo(false)
	  }
	  "recognize sorted list with repeating ints" in {
	    val sorted = List(1,1,3,9,9)
	    isSorted(sorted) must equalTo(true)
	  }
	}
	
}