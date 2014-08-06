import eu.ace_design.island.map._
import eu.ace_design.island.util.{LogSilos, Logger}
import eu.ace_design.island.dsl.DiSLand




object Main extends App with Logger with DiSLand {

  val silo = LogSilos.ROOT

  // Creating the map
  val medium: IslandMap = createIsland shapedAs radial(factor = 1.37) withSize 800 having 600.faces
  medium -> ("./map" as pdf)

}

