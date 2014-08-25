package eu.ace_design.island.map

import eu.ace_design.island.geom.Mesh

/**
 * An IslandMap wraps a geometrical mesh and add properties (i.e., semantics) to each faces
 * @param mesh the geometrical mesh used
 * @param faceProps a propertySet associated to the faces stored in mesh
 * @param vertexProps a propertySet associated to the vertices stored in mesh
 * @param uuid if given, it contains the UUID used to initialise the random generator
 */
class IslandMap private ( val mesh: Mesh,
                          val faceProps: PropertySet   = PropertySet(),
                          val vertexProps: PropertySet = PropertySet(),
                          val uuid: Option[String]     = None) {

  /**
   * copy Method defined to mimic classical case classes
   * @param mesh
   * @param faceProps
   * @param vertexProps
   * @param uuid
   * @return
   */
  def copy(mesh: Mesh = this.mesh, faceProps: PropertySet = this.faceProps, vertexProps: PropertySet = this.vertexProps,
           uuid: Option[String] = this.uuid): IslandMap = new IslandMap(mesh, faceProps, vertexProps, uuid)

  /**
   * Structural equality for maps
   * @param other
   * @return
   */
  override def equals(other: Any) = other match {
    case that: IslandMap => this.mesh == that.mesh && this.faceProps == that.faceProps &&
                              this.vertexProps == that.vertexProps && this.uuid == that.uuid
    case _ => false
  }

  /**
   * HashCode delegated to the List algorithm
   * @return
   */
  override def hashCode(): Int = (mesh :: faceProps :: vertexProps :: uuid :: Nil).hashCode()


}

/**
 * Companion object to hide the private constructor in the class with the apply syntactic sugar
 */
object IslandMap {

  def apply(mesh: Mesh) = new IslandMap(mesh)

}
