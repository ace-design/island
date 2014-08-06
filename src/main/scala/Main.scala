import eu.ace_design.island.geom._
import eu.ace_design.island.map._
import eu.ace_design.island.viewer._
import eu.ace_design.island.util.{LogSilos, Logger}


object Main extends App with Logger {

  val silo = LogSilos.ROOT

  final val OUTPUT_FILE = "./map"
  final val size = Sizes.MEDIUM

  final val MAP_SIZE = size._1
  final val NB_FACES = size._2

  info("Starting the map generation process")

  // Randomly generate the sites used to generate the map
  //val generator = new SquaredGrid(MAP_SIZE)
  //val generator = new RandomGrid(MAP_SIZE)
  val generator = new RelaxedRandomGrid(MAP_SIZE)
  val sites = generator(NB_FACES)

  // Instantiate a mesh builder, and process the random sites to create a mesh
  val meshBuilder = new MeshBuilder(MAP_SIZE)
  val mesh = meshBuilder(sites)

  // Instantiate an Island Builder, and build the map on the previously created mesh
  val mapBuilder = new IslandBuilder {
    //final val ISLAND_SHAPE = DiskShape(size, size.toDouble/2 * 0.85)
    //final val ISLAND_SHAPE = DonutShape(size, size.toDouble/2 * 0.85, size.toDouble/2 * 0.20)
    final val ISLAND_SHAPE = RadialShape(size, 1.87)
    final val WATER_THRESHOLD = 30
    override def size: Int = MAP_SIZE
    override protected val steps: Seq[Process] = Seq(
      IdentifyBorders, IdentifyWaterArea(ISLAND_SHAPE, WATER_THRESHOLD),
      IdentifyLakesAndOcean, IdentifyCoastLine
    )
  }
  val map = mapBuilder(mesh)

  info("End of the map generation process")

  info("Starting the transformation into PDF")
  val transformer = new PDFViewer()
  val result = transformer(map)
  result.renameTo(new java.io.File(OUTPUT_FILE+".pdf"))

  val toOBJ = new OBJViewer()
  val objfile = toOBJ(map)
  objfile.renameTo(new java.io.File(OUTPUT_FILE+".obj"))

  info("PDF file generated!")
}

object Sizes {

  val SMALL = (100, 100)
  val MEDIUM = (625, 625)
  val LARGE = (2048, 4096)

}
