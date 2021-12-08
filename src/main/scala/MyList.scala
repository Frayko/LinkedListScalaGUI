trait MyList[T] {
  def pushBack(data: T): Unit
  def pushFront(data: T): Unit
  def insert(data: T, index: Int): Unit
  def remove(index: Int): Unit
  def get(index: Int): T
  def set(data: T, index: Int): Unit
  def isEmpty: Boolean
  def getSize: Int
  def sort(comparator: Comparator): Unit
}
