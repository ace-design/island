import eu.ace_design.island.geom._
import eu.ace_design.island.viewer._


object Main extends App {

  // constants used by this generation program
  final val OUTPUT_FILE = "/Users/mosser/Desktop/map.svg"
  final val MAP_SIZE = 2048
  final val NB_TILES = 2000

  // Randomly generate the sites used to generate the map
  val generator = new RelaxedRandomGrid(MAP_SIZE)
  val sites = generator(NB_TILES)

  // Instantiate a builder, and process the random sites to create a mesh
  val builder = new MeshBuilder(MAP_SIZE)
  val mesh = builder(sites)

  // Transform the result into a vector graphics
  val transformer = new SVGViewer()
  val result = transformer(mesh)
  result.renameTo(new java.io.File(OUTPUT_FILE))

}