package eu.ace_design.island.map

import eu.ace_design.island.geom.Registry

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
 * @param _contents The internal storage used for the property set
 */
class PropertySet private (private val _contents: Map[Int, Set[Property[_]]]) {

  /**
   * The size of this property set (number of indexes)
   * @return the size of the internal storage
   */
  def size: Int = _contents.size

  /**
   * Extract the properties associated to a given index
   * @param idx the index to look for
   * @return the set of properties associated to idx, the empty set if idx does not exists
   */
  def get(idx: Int): Set[Property[_]] = _contents.getOrElse(idx, Set())

  /**
   * Add a property to a given index, in a functional way.
   *
   * The semantics of the add of the add function is the following:
   *   - there is no property for this index: initialise the index, and store the property
   *   - there is existing properties, different than the one to add: add it to the set of properties
   *   - a property value exists for this index: replace it by the new one.
   *
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
    new PropertySet(newContents)
  }

  /**
   * Add a property to a set of indexes, exploiting the classical '+' operator
   * @param pair c ouple (SetOfIndexes -> properties)
   * @return a new PropertySet, according to the add semantic
   */
  def bulkAdd(pair: (Set[Int], Property[_])): PropertySet = (this /: pair._1) { (acc, i) => acc + (i -> pair._2) }

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

  /**
   * Project this property set according to a given registry.
   *
   * The semantics of the projection is to find all the elements stored in a given registry that satisfy a set of given
   * properties (combined as a conjunction). It is defined as a partial function, first capturing the registry to be
   * used, and then allowing one to identify elements in this registry according to properties.
   *
   * @param reg the registry to be used
   * @param props the property one is looking for
   * @tparam T the type of the elements stored in reg
   * @return a set of T satisfying the properties props and stored in reg.
   */
  def project[T](reg: Registry[T])(props: Set[Property[_]]): Set[T] = {
    val concerned = _contents filter { case (i,existing) => (existing & props) == props } map { _._1 }
    (concerned map { reg(_) }).toSet
  }
}

/**
 * Companion object to mimic case class
 */
object PropertySet { def apply() = new PropertySet(Map()) }

/*********************************************
 ** Properties available in the Island game **
 *********************************************/

case class IsBorder(override val value: Boolean = true) extends Property[Boolean] {
  override val key = "isBorder?"
  def unary_!() = IsBorder(value = ! this.value)
}

case class IsWater(override val value: Boolean = true) extends Property[Boolean] {
  override val key = "isWater?"
  def unary_!() = IsWater(value = ! this.value)
}

case class HasForHeight(override val value: Double = 0.0) extends Property[Double] {
  override val key = "height"
}