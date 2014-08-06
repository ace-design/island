package eu.ace_design.island.dsl

import eu.ace_design.island.geom._
import eu.ace_design.island.map._
import eu.ace_design.island.map.processes._
import eu.ace_design.island.viewer._

/**
 * This trait implements the different syntactic construction to be used to build an Island.
 * An application wishing to build islands should mix this trait to access to the DSL
 */
trait DiSLand {
  // To allow implicit conversions as something explicitly wanted
  import scala.language.implicitConversions


  /**
   * This keyword initialise a default configuration for a map
   * @return
   */
  def createIsland = Configuration()

  /**
   * Implicit transformation to be used when a Configuration must be considered as an IslandMap
   * @param c the given configuration to transform
   * @return the associated IslandMap
   */
  implicit protected def config2map(c: Configuration): IslandMap = c.toMap

  type PointGeneratorDirective = (Int,Int) => Set[Point]
  protected val smoothly: PointGeneratorDirective = (size, faces) => (new RelaxedRandomGrid(size))(faces)
  protected val randomly: PointGeneratorDirective = (size, faces) => (new RandomGrid(size))(faces)
  protected val squarely: PointGeneratorDirective = (size, faces) => (new SquaredGrid(size))(faces)

  type ShapeDirective = (Int, Int) => Process
  protected def disk(surface: Percentage): ShapeDirective = (size, threshold) => {
    IdentifyWaterArea(new DiskShape(size, size.toDouble/2 * surface.value), threshold)
  }
  protected def donut(external: Percentage, lake: Percentage): ShapeDirective = (size, threshold) => {
    IdentifyWaterArea(new DonutShape(size, size.toDouble/2 * external.value, size.toDouble/2 * lake.value), threshold)
  }
  protected def radial(factor: Double): ShapeDirective = (size, threshold) => {
    IdentifyWaterArea(new RadialShape(size, factor), threshold)
  }

  protected val borders: Process        = IdentifyBorders
  protected val lakesAndOceans: Process = IdentifyLakesAndOcean
  protected val coastLine: Process      = IdentifyCoastLine
  protected val defaultProcess = Seq(borders, lakesAndOceans, coastLine)

  /**
   * A configuration contains all the information (variation point configuration) needed to build a Map
   * @param mapSize the size pf the map
   * @param faces the number of faces to generate in a size x size area
   * @param waterThreshold percentage of vertices tagged as water to consider a face as water
   * @param generator the point generator to be used to create the faces
   * @param process the sequence of process to be used to properly build the island
   */
  protected case class Configuration(mapSize: Int        = 1024,
                                     faces: Int          = 1000,
                                     waterThreshold: Int = 30,
                                     generator: PointGeneratorDirective = smoothly,
                                     shape: ShapeDirective = radial(1.87),
                                     process: Seq[Process] = defaultProcess) {

    def withSize(s: Int) = this.copy(mapSize = s)
    def having(f: Int) = this.copy(faces = f)
    def withThreshold(t: Int) = this.copy(waterThreshold = t)
    def distributed(dir: PointGeneratorDirective) = this.copy(generator = dir)
    def shapedAs(s: ShapeDirective) = this.copy(shape = s)
    def builtWith(seq: Seq[Process]) = this.copy(process = seq)

    /**
     * Transform this very specific configuration into an Island map
     * @return the map built using this configuration
     */
    def toMap: IslandMap = {
      val sites = generator(mapSize, faces)
      val meshBuilder = new MeshBuilder(mapSize)
      val mesh = meshBuilder(sites)
      val mapBuilder = new IslandBuilder {
        override def size: Int = mapSize
        override protected val steps: Seq[Process] = shape(mapSize, waterThreshold) +: process
      }
      val map = mapBuilder(mesh)
      map
    }

  }

  // val out = outputFormat(map)
  // out.renameTo(new java.io.File(outputFile+"."+outputFormat.extension))

  /**
   * Syntactic sugar to supports construction such as 100.faces
   * @param i the integer to wrap, returned through the "face" function
   */
  protected class FaceInteger(val i: Int) { def faces = i }
  // Implicit transformation from an Int
  implicit protected def integerToFaceInteger(i: Int) = new FaceInteger(i)

  protected class Percentage(val i: Int) { require(i >=0 && i <= 100); val percent = this; def value = i.toDouble/100 }
  implicit protected def integerToPercentage(i: Int) = new Percentage(i)

  // syntactic sugar to use pre-existing viewers as keywords
  protected val pdf = new PDFViewer()
  protected val svg = new SVGViewer()
  protected val obj = new OBJViewer()




}
