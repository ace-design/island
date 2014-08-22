import eu.ace_design.island.map._
import eu.ace_design.island.util.{LogSilos, Logger}
import eu.ace_design.island.dsl.DiSLand


/**
 * Example of main application exploiting the Island features
 */
object Main extends App with Logger with DiSLand {

  val silo = LogSilos.ROOT

  val small = createIsland shapedAs donut(80.percent, 20.percent) withSize 400 having 1200.faces
  export(small)

  val medium = createIsland shapedAs radial(factor = 1.37) withSize 800 having 1024.faces
  //export(medium)

  val large = createIsland shapedAs radial(factor = 1.07) withSize 2048 having 4096.faces
  //export(large)


  private def export(m: IslandMap, name: String = "./map") {
    m -> (name as pdf)
    m -> (name as obj)
    //m -> (name as json)
  }

}

