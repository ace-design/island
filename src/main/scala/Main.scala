import eu.ace_design.island.map._
import eu.ace_design.island.util.{LogSilos, Logger}
import eu.ace_design.island.dsl.DiSLand


/**
 * Example of main application exploiting the Island features
 */
object Main extends App with Logger with DiSLand {

  val silo = LogSilos.ROOT


  val small = createIsland shapedAs disk(surface= 80.percent) withSize 400 having 800.faces
  export(small)

  val medium = createIsland shapedAs radial(factor = 1.37) withSize 800 having 600.faces
  //export(medium)

  val large = createIsland shapedAs radial(factor = 1.50) withSize 2048 having 4096.faces
  //export(large)


  private def export(m: IslandMap, name: String = "./map") {
    m -> (name as pdf)
    m -> (name as obj)
    m -> (name as json)
  }

}

