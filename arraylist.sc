import scala.reflect.ClassTag

object arraylist {

  class ArrayList[T: ClassTag](maxSize: Int) {
    require(maxSize > 0)

    private val SIZE_FACTOR = 8
    private var data = new Array[T](maxSize)
    private var index = 0
    private var size = maxSize

    def add(t: T): Unit = {
      if (index == size - 1){
        resize()
      }
      data(index) = t
      index += 1
    }

    private def resize(): Unit = {
      size += SIZE_FACTOR
      val origData = data
      data = new Array[T](size)
      for (i <- 0 until origData.length) {
        data(i) = origData(i)
      }
    }

    def get(i: Int): T = {
      if (i < 0 || i > size - 1) throw new Exception("ArrayOutOfBounds")
      else data(i)
    }

    def remove(i: Int): T = {
      if (i < 0 || i > size - 1) throw new Exception("ArrayOutOfBounds")
      val value = data(i)
      for(idx <- i until size - 1) {
        data(idx) = data(idx + 1)
      }
      index -= 1
      value
    }

    def length: Int = index

    def isEmpty: Boolean = index == 0
  }

  val array = new ArrayList[Int](3)
  array.add(1)
  array.add(2)
  array.add(3)
  array.add(4)
  array.get(3)
  array.remove(3)
  array.length
  array.isEmpty
}