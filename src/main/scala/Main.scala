import eu.ace_design.island.geom._
import eu.ace_design.island.map._
import eu.ace_design.island.viewer._
import eu.ace_design.island.util.Logger


object Main extends App with Logger {

  final val OUTPUT_FILE = "./map.pdf"
  //final val MAP_SIZE = 100
  //final val NB_FACES = 100
  //final val MAP_SIZE = 800
  //final val NB_FACES = 800
  final val MAP_SIZE = 2048
  final val NB_FACES = 4000

  logger.info("Starting the map generation process")

  // Randomly generate the sites used to generate the map
  val generator = new RelaxedRandomGrid(MAP_SIZE)
  //val generator = new SquaredGrid(MAP_SIZE)
  val sites = generator(NB_FACES)

  // Instantiate a mesh builder, and process the random sites to create a mesh
  val meshBuilder = new MeshBuilder(MAP_SIZE)
  val mesh = meshBuilder(sites)

  // Instantiate an Island Builder, and build the map on the previously created mesh
  val mapBuilder = new IslandBuilder {
    final val ISLAND_SHAPE = DonutShape(size, size.toDouble/2 * 0.85, size.toDouble/2 * 0.20)
    final val WATER_THRESHOLD = 30
    override def size: Int = MAP_SIZE
    override protected val steps: Seq[Process] = Seq(
      IdentifyBorders, IdentifyWaterArea(ISLAND_SHAPE, WATER_THRESHOLD), IdentifyLakesAndOcean
    )
  }
  val map = mapBuilder(mesh)

  logger.info("End of the map generation process")

  logger.info("Starting the transformation into PDF")
  val transformer = new PDFViewer()
  val result = transformer(map)
  result.renameTo(new java.io.File(OUTPUT_FILE))

  logger.info("PDF file generated!")
}