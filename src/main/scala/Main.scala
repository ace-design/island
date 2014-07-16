import eu.ace_design.island.geom._
import eu.ace_design.island.viewer._


object Main extends App {

  // constants used by this generation program
  final val OUTPUT_FILE = "/Users/mosser/Desktop/map.svg"
  final val MAP_SIZE = 2048
  final val NB_FACES = 2000

  println("** Starting generation process")

  // Randomly generate the sites used to generate the map
  val generator = new RelaxedRandomGrid(MAP_SIZE)
  val sites = generator(NB_FACES)

  // Instantiate a builder, and process the random sites to create a mesh
  val builder = new MeshBuilder(MAP_SIZE)
  val mesh = builder(sites)

  // Transform the result into a PDF file
  val transformer = new SVGViewer()
  val result = transformer(mesh)

  // Move the generated map to my Desktop
  result.renameTo(new java.io.File(OUTPUT_FILE))

  println("** That's all folks")
}