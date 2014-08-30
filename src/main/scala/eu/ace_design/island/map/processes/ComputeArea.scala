package eu.ace_design.island.map.processes

import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
import eu.ace_design.island.map.{HasForArea, IslandMap}

/**
 * The ComputeArea Process compute the area of each face based on the polygon present in the mesh.
 */
object ComputeArea extends Process {

  final val PIXEL_FACTOR: Int = 10 // 1px == PIXEL_FACTOR meters

  override def apply(m: IslandMap): IslandMap = {
    info("Computing faces areas")
    val props = (m.faceProps /: m.faceRefs) { (acc, ref) => acc + (ref -> HasForArea(process(ref, m))) }
    val areas = props.restrictedTo(HasForArea())

    val total = (0.0 /: areas.values) { (acc, v) => acc + v }
    val min = toHectares(areas.values.min)
    val max = toHectares(areas.values.max)
    info(f"total = ${toHectares(total)}%2.2fha, avg=${toHectares(total)/areas.size}%2.2fha")

    m.copy(faceProps = props)
  }

  /**
   * Transform an amount of squared meters into hectares (1km2 = 100ha, 1ha = 10,000m2, ~ 300m x 300m square)
   * @param m2
   * @return
   */
  private def toHectares(m2: Double): Double = m2 / 10000 // 1ha == 10,000 m2

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
