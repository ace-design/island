package eu.ace_design.island.viewer.svg

import java.awt.{Color, BasicStroke, Graphics2D}
import java.awt.geom.Line2D
import eu.ace_design.island.map.resources.Biome
import eu.ace_design.island.map.{RiverFlow, HasForBiome, IslandMap}
import eu.ace_design.island.stdlib.{Colors, Biomes}
import Colors._

/**
 * the BiomeViewer display a map by painting faces according to their biomes, and also rivers (in addition to
 * oceans).
 */
trait BiomeViewer extends SVGViewer {

  protected def draw(m: IslandMap, g: Graphics2D) {
    m.faceRefs foreach { drawABiome(_, m, g) }
    m.edgeRefs foreach { drawAnEdge(_, m, g) }
    if (m.uuid.isDefined) {
      g.setColor(BLACK)
      g.drawString(s"seed: 0x${m.uuid.get.toHexString.toUpperCase}L", 5, m.size - 5)
    }
  }

  protected def drawABiome(idx: Int, map: IslandMap, g: Graphics2D) {
    val path = buildPath(idx, map)
    try {
      val biome = map.faceProps.getValue(idx, HasForBiome())
      g.setColor(biome.color)
    } catch { case e: IllegalArgumentException => g.setColor(WHITE) }
    g.setStroke(new BasicStroke(1f))
    g.draw(path)
    g.fill(path)
  }

  private def drawAnEdge(idx: Int, map: IslandMap, g: Graphics2D) {
    try {
      val flow = map.edgeProps.getValue(idx, RiverFlow()) // throw an exception if no river flows through this edge
      trace(s"edge #$idx with flow $flow")
      val edge = map.edge(idx)
      val p1 = map.vertex(edge.p1)
      val p2 = map.vertex(edge.p2)
      g.setStroke(new BasicStroke(2f * flow))
      g.setColor(MEDIUM_BLUE)
      //g.setColor(BLACK)
      g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y))
    } catch { case e: IllegalArgumentException => } // do nothing if not a river
  }

}

object BiomeViewer extends BiomeViewer

case class FogOfWarViewer(fog: FogOfWar) extends BiomeViewer {
  override val enhancers = Seq(fog)
}