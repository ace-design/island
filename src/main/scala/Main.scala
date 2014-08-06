import eu.ace_design.island.map._
import eu.ace_design.island.util.{LogSilos, Logger}





object Main extends App with Logger with eu.ace_design.island.dsl.DiSLand {

  val silo = LogSilos.ROOT

  // Creating the map
  val medium: IslandMap = createIsland shapedAs radial(factor = 1.87) withSize 600 having 1000.faces

  // medium -> "./medium.pdf" as pdf
  pdf(medium).renameTo(new java.io.File("./medium.pdf"))

}

