import java.awt.Color

import eu.ace_design.island.map._
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

  val large = createIsland shapedAs radial(1.07) usingSeed "9ac771d2-47f7-4037-ad83-919cd4edc1be" withSize 2048 having 4096.faces builtWith Seq(
    flat(40),
    flowing(rivers = 30, distance = 0.4),
    withMoisture(soils.normal, distance = 400),
    usingBiomes()
  )
  export(large)

  val always = createIsland usingSeed "64236166-165d-47f0-a4fd-ed2c443ff834"
  //export(always)


  private def export(m: IslandMap, name: String = "./map") {
    m -> (name as pdf)
    //m -> (name as obj)
    //m -> (name as json)
    m -> (s"$name-height" as heatMap(HasForHeight()))
    m -> (s"$name-moisture" as heatMap(HasForMoisture(), Color.BLUE))
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

