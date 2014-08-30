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

  private def toHectares(m2: Double): Double = m2 / 10000 // 1ha == 10,000 m2

  private def process(faceRef: Int, map: IslandMap): Double = {
    val f = map.face(faceRef)
    val coords = (map.cornerRefs(f) map { i => map.vertex(i) } map { p => new Coordinate(p.x, p.y) }).toSeq
    val linear = coords :+ new Coordinate(coords(0).x, coords(0).y)
    val factory = new GeometryFactory()
    val convexHull = factory.createPolygon(linear.toArray).convexHull
    convexHull.getArea * (PIXEL_FACTOR * PIXEL_FACTOR) // convert squaredPixel to squared meters
  }




}
