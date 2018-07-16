
object set {

  class HashSet[T] {

    // Dummy value to use in HashMap
    private val PRESENT = new Object()
    private val map = new java.util.HashMap[T, Object]

    def add(t: T): Boolean = map.put(t, PRESENT) == null

    def remove(t: T): Boolean = map.remove(t) == PRESENT

    def contains(t: T): Boolean = map.containsKey(t)
  }

  val set = new HashSet[Int]
  set.add(1)
  set.add(2)
  set.contains(1)
  set.remove(1)
  set.contains(1)

}