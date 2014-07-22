package eu.ace_design.island.map

trait Property[T] {
  val key: String
  val value: T
}

case class PropertySet(_contents: Map[Int, Set[Property[_]]]= Map()) {

  def size: Int = _contents.size

  def get(idx: Int): Set[Property[_]] = _contents(idx)

  def +(pair: (Int,Property[_])): PropertySet = {
    val idx = pair._1
    val prop = pair._2
    val newContents: Map[Int, Set[Property[_]]] = _contents.get(idx) match {
      case None => _contents + (idx -> Set(prop))
      case Some(existing) => {
        val sameAs = existing filter {e => e.key == prop.key }
        val without = _contents - idx
        if (sameAs.isEmpty) {
          without + (idx -> (existing + prop))
        } else {
          without + (idx -> ((existing diff sameAs) + prop))
        }
      }
    }
    this.copy(_contents = newContents)
  }

  def check(idx: Int, p: Property[_]): Boolean = _contents.get(idx) match {
    case None => false
    case Some(existing) => existing contains p
  }

}


case class IsWater(override val value: Boolean = true) extends Property[Boolean] {
  override val key = "isWater?"

  def unary_!() = IsWater(value = ! this.value)
}

case class HasForHeight(override val value: Double = 0.0) extends Property[Double] {
  override val key = "height"
}