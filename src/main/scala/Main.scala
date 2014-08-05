import eu.ace_design.island.map._
import eu.ace_design.island.util.{LogSilos, Logger}





object Main extends App with Logger with eu.ace_design.island.dsl.DiSLand {

  val silo = LogSilos.ROOT

  // Creating the map
  val medium: IslandMap = createIsland withSize 800 having 600.faces storedIn "./map-dsl" as pdf

}

