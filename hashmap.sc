object CommonDataStructures{

  trait HashMapInterface[K, V] {

    val maxSize: Int

    def set(k: K, v: V): Unit

    def get(k: K): Option[V]

    def delete(k: K): Option[V]
  }

  // Use linked lists for collisions
  case class HashEntry[K, V](var k: K, var v: V, var next: HashEntry[K,V])

  // TODO: * Handle re-sizing at 75% capacity
  //       * Move from LinkedList to balanced trees
  class HashMap[K, V](override val maxSize: Int) extends HashMapInterface[K,V] {

    require(maxSize > 0)
    var size = 0

    // Array can't delete element or generally resize
    // and ArrayBuffer can't access element at index if empty
    private var data = new Array[HashEntry[K,V]](maxSize)

    for(i <- 0 until maxSize) {
      data(i) = HashEntry(null.asInstanceOf[K], null.asInstanceOf[V], null)
    }

    private def find(key: K, head: HashEntry[K,V]): HashEntry[K,V] = {
      var n = head
      while (n.next != null && n.next.k != key) {
        n = n.next
      }
      n
    }

    override def set(k: K, v: V): Unit = {
      val hash = k.hashCode % maxSize
      val n = find(k, data(hash))
      if (n.next != null) {
        n.next.next = HashEntry[K,V](k, v, null)
      } else {
        data(hash) = HashEntry[K,V](k, v, null)
      }
      size += 1
    }

    override def get(k: K): Option[V] = {
      val hash = k.hashCode % maxSize
      val n = find(k, data(hash))
      if (n.next != null) {
        Some(n.next.v)
      }
      else None
    }

    override def delete(k: K): Option[V] = {
      val hash = k.hashCode % maxSize
      val n = find(k, data(hash))
      if (n.next != null) {
        val value = n.next.v
        n.next = null
        size -= 1
        Some(value)
      }
      None
    }

  }

  var hm = new HashMap[String, Int](2)
  hm.set("foo", 1)
  hm.set("bar", 2)
  hm.size
  hm.delete("foo")
  hm.size
  hm.get("bar")
  hm.get("foo")
  hm.get("foobar")

}