import eu.ace_design.island.map._
import eu.ace_design.island.util.{LogSilos, Logger}
import eu.ace_design.island.dsl.DiSLand


/**
 * Example of main application exploiting the Island features
 */
object Main extends App with Logger with DiSLand {

  val silo = LogSilos.ROOT

  info("Configuring small island")
  val small = createIsland shapedAs donut(80.percent, 20.percent) withSize 400 having 1200.faces
  //export(small)

  info("Configuring medium island")
  val medium = createIsland shapedAs radial(factor = 1.47)
  //export(medium)

  info("Configuring large island")
  val large = createIsland shapedAs radial(factor = 1.07) withSize 2048 having 4096.faces
  //export(large)

  info("Configuring a reproducible island")
  val always = createIsland usingSeed "64236166-165d-47f0-a4fd-ed2c443ff834"
  export(always)


  private def export(m: IslandMap, name: String = "./map") {
    m -> (name as pdf)
    m -> (name as obj)
    //m -> (name as json)
  }

}

