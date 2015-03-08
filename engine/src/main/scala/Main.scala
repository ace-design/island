import java.awt.Color

import eu.ace_design.island.game.{GameBoard, GameBoardBuilder}
import eu.ace_design.island.map._
import eu.ace_design.island.stdlib.Islands
import eu.ace_design.island.stdlib.POIGenerators.WithCreeks
import eu.ace_design.island.util.{LogSilos, Logger}
import eu.ace_design.island.dsl.DiSLand
import eu.ace_design.island.viewer.svg.{FogOfWarViewer, BiomeViewer, FogOfWar}


/**
 * Example of main application exploiting the Island features
 */
object Main extends App with Logger with DiSLand {

  val silo = LogSilos.ROOT

  // Building the island
  val island: IslandMap = Islands.donuts
  export(island)

  // Instantiating the game board
  val pois = Seq(new WithCreeks(10))
  val board: GameBoard = (new GameBoardBuilder(rand = island.random, poiGenerators = pois))(island)
  boardStatistics(board)


  private def export(m: IslandMap, name: String = "./map") {
    import eu.ace_design.island.viewer.svg.{Mappers,Selectors}
    m -> (name as pdf)
    //m -> (name as obj)
    m -> (name as json)
    m -> (s"$name-height" as heatMap(HasForHeight(), Color.RED, Selectors.vertices,  Mappers.faceCenterRef))
    m -> (s"$name-moisture" as heatMap(HasForMoisture(), Color.BLUE))
    m -> (s"$name-pitch" as heatMap(HasForPitch(), Color.DARK_GRAY))
    islandStatistics(m)
  }

  /*private def withFog(m: IslandMap, name: String = "./fog") = {
    val fog = new FogOfWar(factor = 10, visited = Set((0,0), (10,15),(10,16),(11,15)), pois = Set((103.0, 154.0)), size = m.size)
    val viewer = FogOfWarViewer(fog)
    viewer(m).renameTo(new java.io.File(name+".svg"))
  } */ // TODO move this code to the arena



  protected def islandStatistics(m: IslandMap) = {
    info("Available statistics for the Island")
    m.stats match {
      case None =>
      case Some(d) => d.toSeq sortBy { _._1.toString  } foreach { case (stat, value) => info(s"  - $stat => $value") }
    }
  }

  protected def boardStatistics(b: GameBoard) = {
    info(s"Available statistics for the Game board (${b.tiles.keySet.size} tiles)")
    b.contents foreach { case (res, amount) => info(f"  - ${res}%-10s => $amount") }
    b.pois foreach { case (loc, pois) => info(s"  $loc: $pois") }
  }
}

