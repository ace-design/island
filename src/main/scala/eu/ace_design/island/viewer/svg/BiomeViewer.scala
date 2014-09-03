package eu.ace_design.island.viewer.svg

import java.awt.{Color, BasicStroke, Graphics2D}
import java.awt.geom.Line2D

import eu.ace_design.island.map.{RiverFlow, ExistingBiomes, HasForBiome, IslandMap}
import eu.ace_design.island.viewer.ColorBrewer._

/**
 * the BiomeViewer display a map by painting faces according to their biomes, and also rivers (in addition to
 * oceans).
 */
object BiomeViewer extends SVGViewer {

  protected def draw(m: IslandMap, g: Graphics2D) {
    m.faceRefs foreach { drawABiome(_, m, g) }
    m.edgeRefs foreach { drawAnEdge(_, m, g) }
    if (m.uuid.isDefined) {
      g.setColor(BLACK)
      g.drawString(s"seed: ${m.uuid.get}", 5, m.size - 5)
    }
  }

  protected def drawABiome(idx: Int, map: IslandMap, g: Graphics2D) {
    val path = buildPath(idx, map)
    try {
      val biome = map.faceProps.getValue(idx, HasForBiome())
      g.setColor(biomePalette(biome))
    } catch { case e: IllegalArgumentException => g.setColor(WHITE) }
    g.setStroke(new BasicStroke(1f))
    g.draw(path)
    g.fill(path)
  }

  private def drawAnEdge(idx: Int, map: IslandMap, g: Graphics2D) {
    try {
      val flow = map.edgeProps.getValue(idx, RiverFlow()) // throw an exception if no river flows through this edge
      debug(s"edge #$idx with flow $flow")
      val edge = map.edge(idx)
      val p1 = map.vertex(edge.p1)
      val p2 = map.vertex(edge.p2)
      g.setStroke(new BasicStroke(2f * flow))
      g.setColor(MEDIUM_BLUE)
      g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y))
    } catch { case e: IllegalArgumentException => } // do nothing if not a river
  }


  private def biomePalette(biome: ExistingBiomes.Biome): Color = {
    import eu.ace_design.island.map.ExistingBiomes._
    biome match {
      /** Water faces **/
      case GLACIER => LIGHT_BLUE
      case LAKE    => MEDIUM_BLUE
      case OCEAN   => DARK_BLUE

      /** Dry biomes: beach, deserts and alpine rocks **/
      case BEACH               => LIGHT_YELLOW
      case SUB_TROPICAL_DESERT => MEDIUM_YELLOW
      case TEMPERATE_DESERT    => DARK_YELLOW
      case ALPINE              => DARK_GREY

      /** Dry biomes: grassland, shrubland and tundra **/
      case GRASSLAND => LIGHT_ORANGE
      case SHRUBLAND => MEDIUM_ORANGE
      case TUNDRA    => DARK_ORANGE

      /** Half-wet biomes: forests and taiga **/
      case TROPICAL_SEASONAL_FOREST   => ULTRA_LIGHT_GREEN
      case TEMPERATE_RAIN_FOREST      => LIGHT_GREEN
      case TROPICAL_RAIN_FOREST       => DARK_GREEN

      case TEMPERATE_DECIDUOUS_FOREST => MEDIUM_GREEN
      case TAIGA                      => ULTRA_DARK_GREEN

      /** Wet biomes: mangroves and snow  **/
      case MANGROVE => BROWN
      case SNOW     => WHITE

      /** Terra incognita **/
      case _ => BLACK
    }
  }
}