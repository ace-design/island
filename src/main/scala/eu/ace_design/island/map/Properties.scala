package eu.ace_design.island.map

/**
 * A property bound a value to an immutable key.  The trait is sealed, and cannot be implemented outside of this file.
 * @tparam T the type of value
 */
sealed trait Property[T] {
  // The key associated to this property
  val key: String
  // the value (a T)
  val value: T
}

/**
 * A PropertySet associate to a given index a set of properties
 * @param _contents the internal storage
 */
case class PropertySet(_contents: Map[Int, Set[Property[_]]]= Map()) {

  /**
   * The size of this property set (number of indexes)
   * @return the size of the internal storage
   */
  def size: Int = _contents.size

  /**
   * Extract the properties associated to a given index
   * @param idx the index to look for
   * @return the set of properties associated to idx, a NoSuchElementException if idx does not exists
   */
  def get(idx: Int): Set[Property[_]] = _contents(idx)

  /**
   * Add a property to a given index, in a functional way. the semantics of the add of the add function is the
   * following:
   *   - there is no property for this index: initialise the index, and store the property
   *   - there is existing properties, different than the one to add: add it to the set of properties
   *   - a property value exists for this index: replace it by the new one.
   * @param pair a couple (index -> Property) to be added in the property set
   * @return a new property set, according to the add semantics.
   */
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

  /**
   * Check if a property is associated to a given index
   * @param idx the index to check
   * @param p the property one is looking for
   * @return a boolean indicating if the index is associated to  the property.
   */
  def check(idx: Int, p: Property[_]): Boolean = _contents.get(idx) match {
    case None => false
    case Some(existing) => existing contains p
  }

}

/*********************************************
 ** Properties available in the Island game **
 *********************************************/

case class IsWater(override val value: Boolean = true) extends Property[Boolean] {
  override val key = "isWater?"

  def unary_!() = IsWater(value = ! this.value)
}

case class HasForHeight(override val value: Double = 0.0) extends Property[Double] {
  override val key = "height"
}