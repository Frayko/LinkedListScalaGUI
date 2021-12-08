class IntegerType() extends TypeBuilder {
  private var range = 10000

  def this(range: Int) {
    this()
    this.range = range
  }

  override def typeName = "Integer"

  override def create: Int = (Math.random * range).toInt

  override def typeComparator: Comparator = (o1: Any, o2: Any) => o1.asInstanceOf[Int] - o2.asInstanceOf[Int]
}
