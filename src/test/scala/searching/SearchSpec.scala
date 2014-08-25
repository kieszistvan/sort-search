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

  "Search#dfs" should {
    "return the dfs traversal list" in {
      val g: List[(Symbol, List[Int])] =
        List(
          'A -> List(1, 5, 6),
          'B -> List(0, 4, 5),
          'C -> List(5, 7),
          'D -> List(5),
          'E -> List(1, 6),
          'F -> List(0, 1, 2, 3),
          'G -> List(0, 4),
          'H -> List(2))

      dfs(g) must equalTo(List('A, 'B, 'E, 'G, 'F, 'C, 'H, 'D))
    }
  }

}