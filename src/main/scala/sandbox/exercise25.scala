package sandbox

object SuperAdder {
  def add(items: List[Int]): Int = items.foldLeft(0)(_ + _)
}
