package eu.ace_design.island.map

import eu.ace_design.island.geom.Mesh

/**
 * An IslandMap wraps a geometrical mesh and add properties (i.e., semantics) to each faces
 * @param mesh the geometrical mesh used
 * @param faceProps a propertySet associated to the faces stored in mesh
 * @param vertexProps a propertySet associated to the vertices stored in mesh
 * @param uuid if given, it contains the UUID used to initialise the random generator
 */
case class IslandMap(mesh: Mesh,
                     faceProps: PropertySet   = PropertySet(),
                     vertexProps: PropertySet = PropertySet(),
                     uuid: Option[String]     = None) {

}
