object TypeFactory {
  def getTypeNameList: List[String] = {
    var list = List[String]()
    for (t <- MyTypes.values) {
      list = list :+ String.valueOf(t)
    }
    list
  }

  def getBuilder(t : MyTypes.myType): TypeBuilder = t match {
    case MyTypes.Integer => new IntegerType
    case MyTypes.String => new StringType
    case _ => System.out.println("Wrong type, return null")
      null
  }
}
