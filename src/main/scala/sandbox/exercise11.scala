package sandbox.exercise11

final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int) =
    GCounter(counters.updated(machine, counters.getOrElse(machine, 0) + amount))

  def merge(that: GCounter): GCounter =
    GCounter((counters.keySet ++ that.counters.keySet)
      .map(key => key -> (counters.getOrElse(key, 0) max that.counters.getOrElse(key, 0)))
      .toMap)

  def total: Int = counters.values.sum
}
