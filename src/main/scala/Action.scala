trait Action[T] {
  def toDo(data: T): Unit
}
