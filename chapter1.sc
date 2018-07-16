import scala.collection.mutable

object Chapter1 {

  /* Question 1 */

  // O(n^2) implementation
  def strIsUnique(s: String): Boolean = {
    for ((c1, i) <- s.zipWithIndex) {
      for ((c2, j) <- s.zipWithIndex) {
        if (i != j) {
          if (c1 == c2) return false
        }
      }
    }
    return true
  }

  strIsUnique("")
  strIsUnique("a")
  strIsUnique("abc")
  strIsUnique("abca")

  def strIsUniqueBetter(s: String): Boolean = {
    if (s.length > 128) return false

    val charCountList = new mutable.ArrayBuffer[Boolean](128)
    for ((c, i) <- s.zipWithIndex) {
      if (charCountList.lift(c.toInt).getOrElse(false)) {
        return false
      }
      charCountList.insert(c.toInt, true)
    }
    return true
  }

  strIsUniqueBetter("")
//  strIsUniqueBetter("a")
//  strIsUniqueBetter("abc")
//  strIsUniqueBetter("abca")


  /* Question 2 */

  // O(n) solution in O(n) space
  def isPermutation(s1: String, s2: String): Boolean = {
    if(s1.length != s2.length) return false

    val s1Map = new mutable.HashMap[Char, Integer]()
    val s2Map = new mutable.HashMap[Char, Integer]()

    for((c, i) <- s1.zipWithIndex) {
      if (s1Map.isDefinedAt(c)){
        s1Map.update(c, s1Map(c) + 1)
      } else {
        s1Map(c) = 1
      }
    }
    for((c, i) <- s2.zipWithIndex) {
      if (s2Map.isDefinedAt(c)){
        s2Map.update(c, s2Map(c) + 1)
      } else {
        s2Map(c) = 1
      }
    }

    if (s1Map == s2Map) return true else false
  }

  isPermutation("", "")
  isPermutation("abc", "bac")
  isPermutation("aaa", "bbb")

  // O(n log(n)) in O(1) space
  def isPermutationWorst(s1: String, s2: String): Boolean = {
    if(s1.length != s2.length) return false else s1.sorted == s2.sorted
  }

  isPermutationWorst("", "")
  isPermutationWorst("abc", "bac")
  isPermutationWorst("aaa", "bbb")



  /* Question 6 */

  /* O(n) time; non-functional */
  def compressString(s: String): String = {

    val buf = new mutable.StringBuilder()
    var count = 0
    for (i <- s.indices) {
      count += 1
      if (i + 1 >= s.length || s.charAt(i) != s.charAt(i + 1)) {
        buf.append(s.charAt(i))
        buf.append(count.toString)
        count = 0
      }
    }
    if (buf.length < s.length) buf.toString() else s
  }

  compressString("aaaabbbbcc")
  compressString("abc")


  def compressString2(s: String): String = {
    var prevChar = s.head
    var prevCount = 1
    var res = ""
    for (c <- s.tail) {
      if (c != prevChar) {
        res = res + prevChar + prevCount
        prevCount = 1

      } else {
        prevCount += 1
      }
      prevChar = c
    }
    res = res + prevChar + prevCount

    if (res.length < s.length) res else s
  }

  compressString2("aaaaaabbbbbcc")



  /* Question 7 */
  type Matrix[T] = Array[Array[T]]

  def rotateImage[T](matrix: Matrix[T]): Matrix[T] = {
    def rotateLayer(layer: Int): Unit = {
      val boundary = matrix.size - 1
      for (i <- layer until boundary) {
        val topLeft = matrix(layer)(i)

        val topRight = matrix(i)(boundary)
        matrix(i)(boundary) = topLeft

        val bottomRight = matrix(boundary)(boundary - i + layer)
        matrix(boundary)(boundary - i + layer) = topRight

        val bottomLeft = matrix(boundary - i + layer)(layer)
        matrix(boundary - i + layer)(layer) = bottomRight

        matrix(layer)(i) = bottomLeft

      }
    }

    for (layer <- 0 to (matrix.size / 2) - 1) {
      rotateLayer(layer)
    }
    matrix
  }

  val matrix: Matrix[Int] = Array(
    Array[Int](1,2),
    Array[Int](3,4)
  )
  rotateImage[Int](matrix)

}