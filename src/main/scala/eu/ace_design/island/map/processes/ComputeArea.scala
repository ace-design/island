package eu.ace_design.island.map.processes

import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
import eu.ace_design.island.map.{HasForArea, IslandMap}
import eu.ace_design.island.map.resources.PIXEL_FACTOR

/**
 * The ComputeArea Process compute the area of each face based on the polygon present in the mesh.
 */
object ComputeArea extends Process {

  override def apply(m: IslandMap): IslandMap = {
    info("Computing faces areas")
    val props = (m.faceProps /: m.faceRefs) { (acc, ref) => acc + (ref -> HasForArea(process(ref, m))) }
    m.copy(faceProps = props)
  }

  /**
   * Compute the area of a given face by computing the convex hull of its border and then the associated area
   *
   * The area is computed in pixels, we rely on PIXEL_FACTOR to transform 1px2 into squared meters
   *
   * @param faceRef the face to process
   * @param map the map, containing the edges and the vertices
   * @return the area of the face located at faceRef in map, in squared meters
   */
  private def process(faceRef: Int, map: IslandMap): Double = {
    val f = map.face(faceRef)
    val coordinates = map.convexHull(f) map { p => new Coordinate(p.x, p.y) }
    val convexHull = new GeometryFactory().createPolygon(coordinates.toArray)
    convexHull.getArea * (PIXEL_FACTOR * PIXEL_FACTOR) // convert squaredPixel to squared meters
  }




}
