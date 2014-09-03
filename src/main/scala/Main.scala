import java.awt.Color

import eu.ace_design.island.map._
import eu.ace_design.island.map.processes.AssignPitch
import eu.ace_design.island.util.{LogSilos, Logger}
import eu.ace_design.island.dsl.DiSLand


/**
 * Example of main application exploiting the Island features
 */
object Main extends App with Logger with DiSLand {

  val silo = LogSilos.ROOT

  val small = createIsland shapedAs donut(80.percent, 20.percent) withSize 400 having 1200.faces
  //export(small)

  val medium = createIsland shapedAs radial(factor = 1.47)
  //export(medium)

  val large = createIsland shapedAs radial(1.07) usingSeed 0x9AC771d247f74037L withSize 2048 having 4096.faces builtWith Seq(
    //flatDistribution(40), // culminating peak is at ~400meters
    plateau(40), AssignPitch,
    flowing(rivers = 30, distance = 0.4),
    withMoisture(soils.normal, distance = 400),
    usingBiomes()
  )
  //export(large)

  val always = createIsland usingSeed 0x64236166165d47F0L
  export(always)


  private def export(m: IslandMap, name: String = "./map") {
    import eu.ace_design.island.viewer.svg.{Mappers,Selectors}
    m -> (name as pdf)
    m -> (name as obj)
    //m -> (name as json)
    m -> (s"$name-height" as heatMap(HasForHeight(), Color.RED, Selectors.vertices,  Mappers.faceCenterRef))
    m -> (s"$name-moisture" as heatMap(HasForMoisture(), Color.BLUE))
    m -> (s"$name-pitch" as heatMap(HasForPitch(), Color.DARK_GRAY))
    statistics(m)
  }

  protected def statistics(m: IslandMap) = {
    info("Available statistics")
    m.stats match {
      case None =>
      case Some(d) => d.toSeq sortBy { _._1.toString  } foreach { case (stat, value) => info(s"  - $stat => $value") }
    }
  }
}

