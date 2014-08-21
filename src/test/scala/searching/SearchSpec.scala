package searching

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SearchSpec extends Specification {
  import Search._

  "Search#binary" should {
    "return -1 when searching in empty list" in {
      val empty = List()
      binary(empty, 1) must equalTo(-1)
    }
    "return index of element in the list if found" in {
      val index = binary(List(1, 12, 18, 22, 24, 28, 33, 122, 888), 28)
      index must equalTo(5)
    }
  }

}