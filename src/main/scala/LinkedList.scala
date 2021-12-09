import java.io.{File, FileInputStream, FileOutputStream, IOException, ObjectInputStream, ObjectOutputStream, Serializable}
import java.util


class LinkedList[T]() extends MyList[T] with Serializable {
  private var root: Node = null
  private var size: Int = 0

  class Node extends Serializable {
    var data: T = _
    var prev: Node = null
    var next: Node = null

    def this(data: T) {
      this()
      this.data = data
      this.prev = null
      this.next = null
    }

    def this(data: T, prev: Node, next: Node) {
      this()
      this.data = data
      this.prev = prev
      this.next = next
    }

    override def toString: String = "Node{" + "data=" + data + '}'
  }

  override def pushBack(data: T): Unit = {
    if (size == 0) this.root = new Node(data)
    else {
      var tmp: Node = this.root

      while (tmp.next != null) tmp = tmp.next
      tmp.next = new Node(data, tmp, null)
    }
    size += 1
  }

  override def pushFront(data: T): Unit = {
    this.root = new Node(data, null, this.root)
    if (this.root.next != null) this.root.next.prev = this.root
    size += 1
  }

  @throws[NullPointerException]
  override def insert(data: T, index: Int): Unit = {
    if (index > size || index < 0) throw new NullPointerException("Выход за границы списка")
    if (index == 0) pushFront(data)
    else if (index == size) pushBack(data)
    else {
      var buf: Node = this.root
      var i: Int = 0
      while (i < index - 1) {
        i += 1
        buf = buf.next
      }
      buf.next = new Node(data, buf, buf.next)
      if (buf.next.next != null) buf.next.next.prev = buf.next
      size += 1
    }
  }

  @throws[NullPointerException]
  override def remove(index: Int): Unit = {
    if (index >= size || index < 0) throw new NullPointerException("Выход за границы списка")
    if (index == 0) if (size == 1) this.root = null
    else {
      this.root = this.root.next
      this.root.prev = null
    }
    else {
      var buf: Node = this.root
      var i: Int = 0
      while (i < index - 1) {
        i += 1
        buf = buf.next
      }
      buf.next = buf.next.next
      if (buf.next != null) buf.next.prev = buf
    }
    size -= 1
  }

  @throws[NullPointerException]
  private def getNode(index: Int): Node = {
    if (index >= size || index < 0) throw new NullPointerException("Выход за границы списка")
    var buf: Node = this.root
    var i: Int = 0
    while (i != index) {
      i += 1
      buf = buf.next
    }
    buf
  }

  @throws[NullPointerException]
  override def get(index: Int): T = {
    if (index >= size || index < 0) throw new NullPointerException("Выход за границы списка")
    var buf: Node = this.root
    var i: Int = 0
    while (i != index) {
      i += 1
      buf = buf.next
    }
    buf.data
  }

  @throws[NullPointerException]
  override def set(data: T, index: Int): Unit = {
    if (index >= size || index < 0) throw new NullPointerException("Выход за границы списка")
    var buf: Node = this.root
    var i: Int = 0
    while (i != index) {
      i += 1
      buf = buf.next
    }
    buf.data = data
  }

  override def isEmpty: Boolean = size == 0

  override def getSize: Int = size

  override def sort(comparator: Comparator): Unit = {
    this.root = mergeSort(this.root, comparator)
    this.root.prev = null
  }

  def mergeSort(node: Node, comparator: Comparator): Node = {
    if (node == null || node.next == null) return node
    var x: Node = node
    var y: Node = null
    val slow: Node = split(node)
    y = slow.next
    slow.next = null
    x = mergeSort(x, comparator)
    y = mergeSort(y, comparator)
    val newNode = merge(x, y, comparator)
    newNode
  }

  def split(node: Node): Node = {
    var slow: Node = node
    var fast: Node = node.next
    while (fast != null) {
      fast = fast.next
      if (fast != null) {
        slow = slow.next
        fast = fast.next
      }
    }
    slow
  }

  def merge(x: Node, y: Node, comparator: Comparator): Node = {
    var _x = x
    var _y = y
    val merged: Node = new Node()
    var temp: Node = merged
    while (_x != null && _y != null) {
      if (comparator.compare(_x.data, _y.data) < 0) {
        temp.next = _x
        _x.prev = temp
        _x = _x.next
      }
      else {
        temp.next = _y
        _y.prev = temp
        _y = _y.next
      }
      temp = temp.next
    }
    while (_x != null) {
      temp.next = _x
      _x.prev = temp
      _x = _x.next
      temp = temp.next
    }
    while (_y != null) {
      temp.next = _y
      _y.prev = temp
      _y = _y.next
      temp = temp.next
    }
    merged.next
  }

  def forEach(action: Action[T]): Unit = {
    if (size > 0) {
      var node: Node = this.root
      do {
        action.toDo(node.data)
        node = node.next
      } while (node != null)
    }
    else println("Нет элементов в массиве")
  }

  def iterator: util.Iterator[T] = new util.Iterator[T]() {
    var counter: Int = 0
    var buf: Node = root

    override def hasNext: Boolean = this.counter < size

    override def next: T = {
      if ({
        counter += 1; counter - 1
      } != 0) buf = buf.next
      buf.data
    }
  }

  @throws[IOException]
  def save(file: File): Unit = {
    val oos: ObjectOutputStream = new ObjectOutputStream(new FileOutputStream(file, false))
    oos.writeInt(this.size)
    oos.writeObject(this.root)
    oos.close()
  }

  @throws[IOException]
  @throws[ClassNotFoundException]
  def load(file: File): Unit = {
    val ois: ObjectInputStream = new ObjectInputStream(new FileInputStream(file))
    this.root = null
    this.size = 0
    this.size = ois.readInt()
    this.root = ois.readObject.asInstanceOf[Node]
  }
}
