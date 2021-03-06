package eu.ace_design.island.dsl

import eu.ace_design.island.geom._
import eu.ace_design.island.geom.generators.{SquaredGrid, RelaxedRandomGrid, RandomGrid}
import eu.ace_design.island.map._
import eu.ace_design.island.map.processes._
import eu.ace_design.island.map.resources.{ExploitationDistribution, WhittakerDiagram}
import eu.ace_design.island.stdlib.{WhittakerDiagrams, StandardExploitationDistribution}
import eu.ace_design.island.viewer._
import eu.ace_design.island.viewer.svg.{HeatMap, BiomeViewer}

import scala.util.Random

/**
 * This trait implements the different syntactic construction to be used to build an Island.
 * An application wishing to build islands should mix this trait to access to the DSL
 */
trait DiSLand {
  // To allow implicit conversions as something explicitly wanted
  import scala.language.implicitConversions


  // This keyword initialise a default configuration for a map
  def createIsland = Configuration()

  /**
   * Implicit transformation to be used when a Configuration must be considered as an IslandMap
   * @param c the given configuration to transform
   * @return the associated IslandMap
   */
  implicit protected def config2map(c: Configuration): IslandMap = c.toMap

  /**
   * A PointGeneratorDirective uses the map size and the number of expected faces to distribute points on the grid.
   * For random grids, it also uses a random point generator (initialized thanks to the UUID in the configuration)
   */
  protected type PointGeneratorDirective = (Int, Int, Random) => Set[Point]
  protected val smoothly: PointGeneratorDirective = (size, faces, rnd) => (new RelaxedRandomGrid(size,rnd))(faces)
  protected val randomly: PointGeneratorDirective = (size, faces, rnd) => (new RandomGrid(size, rnd))(faces)
  protected val squarely: PointGeneratorDirective = (size, faces, _) => (new SquaredGrid(size))(faces)

  /**
   * A shape directive relies on the map size and the water threshold to be used to determine water faces.
   * Random parameter (based on the configuration UUID) is used when random shape are used in the map.
   */
  protected type ShapeDirective = (Int, Int, Random) => Process

  // The disk shape is defined as "disk(surface = 80.percent)"
  protected def disk(surface: Percentage): ShapeDirective = (size, threshold, _) => {
    IdentifyWaterArea(DiskShape(size, size.toDouble/2 * surface.value), threshold)
  }

  // the donut shape is defined as "donut(external = 80.percent, lake = 10.percent)"
  protected def donut(external: Percentage, lake: Percentage): ShapeDirective = (size, threshold, _) => {
    IdentifyWaterArea(DonutShape(size, size.toDouble/2 * external.value, size.toDouble/2 * lake.value), threshold)
  }

  // the Ellipsis shape is defined as "ellipsis(x = 80.percent, y = 10.percent)" (x == y == 100 => circular)
  protected def ellipsis(x: Percentage, y: Percentage, theta: Int = 0): ShapeDirective = (size, threshold, _) => {
    IdentifyWaterArea(EllipsisShape(size, x.value, y.value, math.toRadians(theta)), threshold)
  }

  protected def oz(): ShapeDirective = (size, threshold, _) => IdentifyWaterArea(OzShape(size), threshold)



  // the radial shape is defined as "radial(factor = 1.57)"
  protected def radial(factor: Double): ShapeDirective = (size, threshold, random) => {
    IdentifyWaterArea(RadialShape(size, factor, random), threshold)
  }

  /**
   * functions as syntactical element to instantiate map building processes
   */
  // Elevations
  def flatDistribution(culmination: Double): Process =
    DistributeElevation(mapper = ElevationMappers.distance, elevator = ElevationDistributions.flat(culmination))

  def plateau(culmination: Double): Process =
    AssignElevation(mapper = ElevationMappers.distance, ElevationFunctions.plateau(culmination))

  // Rivers
  protected def flowing(rivers: Int, distance: Double): Process = GenerateRivers(rivers, distance)

  // moisture
  object soils extends Enumeration {
    type kind = Value
    val dry, normal, wet = Value
  }
  protected def withMoisture(soil: soils.kind, distance: Int = 100): Process = soil match {
    case soils.dry    => AssignMoisture(MoisturePropagation.dry(distance))
    case soils.normal => AssignMoisture(MoisturePropagation.linear(distance))
    case soils.wet    => AssignMoisture(MoisturePropagation.wet(distance))
  }

  protected def usingBiomes(diagram: WhittakerDiagram = WhittakerDiagrams.complete): Process = AssignBiomes(diagram)

  protected def withResources(distribution: ExploitationDistribution = StandardExploitationDistribution): Process =
    AssignExploitationConditions(distribution)

  // the initialisation process used to build island, **always** executed
  private val initProcess: Seq[Process] = Seq(
    IdentifyBorders, AlignVertexWaterBasedOnFaces, IdentifyLakesAndOcean,
    IdentifyCoastLine, MinimalDistanceToCoast, ComputeArea)

  // the process used by default
  private val defaultProcess: Seq[Process] = Seq(
    plateau(30.0), AssignPitch,
    flowing(rivers = 10, distance = 0.4),
    withMoisture(soils.normal),
    usingBiomes(), withResources()
  )

  /**
   * A configuration contains all the information (variation point configuration) needed to build a Map
   * @param mapSize the size pf the map
   * @param faces the number of faces to generate in a size x size area
   * @param waterThreshold percentage of vertices tagged as water to consider a face as water
   * @param generator the point generator to be used to create the faces
   * @param process the sequence of process to be used to properly build the island
   * @param uuid an unique identifier (a Java UUID) used to initialize the random generator (if used)
   */
  protected case class Configuration(mapSize: Int        = 800,
                                     faces: Int          = 2048,
                                     waterThreshold: Int = 30,
                                     generator: PointGeneratorDirective = smoothly,
                                     shape: ShapeDirective = radial(1.27),
                                     process: Seq[Process] = defaultProcess,
                                     uuid: Long          = Random.nextLong()) {

    // The different keywords to be used to update the configuration with specific values

    def withSize(s: Int)      = this.copy(mapSize = s)
    def having(f: Int)        = this.copy(faces = f)
    def withThreshold(t: Int) = this.copy(waterThreshold = t)
    def distributed(dir: PointGeneratorDirective) = this.copy(generator = dir)
    def shapedAs(s: ShapeDirective)  = this.copy(shape = s)
    def builtWith(seq: Seq[Process]) = this.copy(process = seq)
    def usingSeed(s: Long) = this.copy(uuid = s)


    /**
     * Transform this very specific configuration into an Island map
     * @return the map built using this configuration
     */
    def toMap: IslandMap = {
      val random = new Random(uuid)
      val sites = generator(mapSize, faces, random)
      val meshBuilder = new MeshBuilder(mapSize)
      val mesh = meshBuilder(sites)
      val mapBuilder = new IslandBuilder {
        override def size: Int = mapSize
        override protected val steps: Seq[Process] =
          (shape(mapSize, waterThreshold, random) +: (initProcess ++ process)) :+ ComputeStatistics
      }
      mapBuilder(mesh, random).copy(uuid = Some(uuid))
    }
  }

  /**
   * Syntactic sugar to supports construction such as 100.faces
   * @param i the integer to wrap, returned through the "face" function
   */
  protected class FaceInteger(val i: Int) { def faces = i }
  // Implicit transformation from an Int
  implicit protected def integerToFaceInteger(i: Int): FaceInteger = new FaceInteger(i)

  /**
   * Syntactic sugar to support construction such as 80.percent (wasn't able to overload %)
   * @param i th integer to wrap, in [0,100]
   */
  protected class Percentage(val i: Int) { require(i >=0 && i <= 100); val percent = this; def value = i.toDouble/100 }
  implicit protected def integerToPercentage(i: Int): Percentage = new Percentage(i)
  implicit protected def percentageToDouble(p: Percentage): Double = p.value

  /**
   * Syntactic elements to support the "island -> ("fileName" as outputFormat) construction
   */

  protected val pdf = PDFViewer()
  protected val svg = eu.ace_design.island.viewer.svg.BiomeViewer
  protected val obj =  OBJViewer
  protected val json = JsonViewer
  protected def heatMap(p: Property[Double], c: java.awt.Color = java.awt.Color.RED,
                         selector: IslandMap => PropertySet = eu.ace_design.island.viewer.svg.Selectors.faces,
                         mapper: (IslandMap, Int) => Int = eu.ace_design.island.viewer.svg.Mappers.faceRef) =
    PDFViewer(HeatMap(p, c, java.awt.Color.BLACK, selector, mapper))

  // For a given map, one can obtain the file generated by the viewer and the expected file to be used as output
  protected type OutputFileFormatter = IslandMap => (String, java.io.File)
  implicit def stringToEnrichedString(s: String): EnrichedString = new EnrichedString(s)
  protected class EnrichedString(s: String) {
    // This function create a function able to process a map and produce the associated OutputFileFormatter
    def as(viewer: Viewer): OutputFileFormatter  = (map) => (s"$s.${viewer.extension}", viewer(map))
  }
  // Enriching the IslandMap class to add the -> operator, used to store the map into a given file.
  implicit def islandMapToEnrichedIslandMap(m: IslandMap): EnrichedIslandMap = new EnrichedIslandMap(m)
  protected class EnrichedIslandMap(map: IslandMap) {
    def ->(out: IslandMap => (String, java.io.File)) {
      val result = out(map)
      result._2.renameTo(new java.io.File(result._1))
    }
  }

}
