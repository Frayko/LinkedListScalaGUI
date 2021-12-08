class StringType() extends TypeBuilder {
  private var range = 10

  def this(range: Int) {
    this()
    this.range = range
  }

  override def typeName = "String"

  override def create: String = {
    var numberOfChar = 0
    val s = new StringBuilder
    for (i <- 0 until range) {
      if (Math.random * 1 < 0.5) numberOfChar = (Math.random * 25 + 65).round.toInt
      else numberOfChar = (Math.random * 25 + 97).round.toInt
      s.append(numberOfChar.toChar)
    }
    s.toString
  }

  override def typeComparator: Comparator = (o1: Any, o2: Any) => o1.asInstanceOf[String].compareTo(o2.asInstanceOf[String])
}
