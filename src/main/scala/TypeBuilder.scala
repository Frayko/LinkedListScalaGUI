trait TypeBuilder {
  def typeName: String
  def create: Any
  def typeComparator: Comparator
}
