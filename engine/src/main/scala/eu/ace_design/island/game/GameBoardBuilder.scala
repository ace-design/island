package eu.ace_design.island.game

import eu.ace_design.island.geom.Point
import eu.ace_design.island.map._
import eu.ace_design.island.map.resources.Soils.Soil
import eu.ace_design.island.map.resources.Conditions.Condition
import eu.ace_design.island.map.resources.{NoResource, Biome, PrimaryResource}
import eu.ace_design.island.util.{LogSilos, Logger}
import scala.util.Random


/**
 * This class is used to bind a Generated map to a GameBoard
 *
 * Please do look at the commit date before complaining about the code quality ^^
 *
 * @param chunk the size of each tile (map.size must be a factor of chunk when applied)
 * @param rand a random generator, to be forwarded to the Biome2Resource mapper
 */
class GameBoardBuilder(chunk: Int = DEFAULT_TILE_UNIT,
                       poiGenerators: Seq[POIGenerator] = Seq(),
                       rand: Random = new Random()) extends Logger {

  override val silo = LogSilos.BOARD_GEN

  val locator = new TileLocator(chunk)

  /**
   * Build a game board on top of a given map
   * @param map the map to use as input
   * @return the associated game board
   */
  def apply(map: IslandMap): GameBoard = {
    require(map.size % chunk == 0, "the size of the island must be compatible with the size of a tile")
    info("Starting GameBoard building process")
    // Extracting relevant information from the map
    val biomes     = map.faceProps.restrictedTo(HasForBiome())
    val soils      = map.faceProps.restrictedTo(HasForSoil())
    val conditions = map.faceProps.restrictedTo(HasForCondition())
    val areas      = map.faceProps.restrictedTo(HasForArea())
    val pitches    = map.faceProps.restrictedTo(HasForPitch())
    val moistures  = map.faceProps.restrictedTo(HasForMoisture())

    // Computing the resources associated to tile, face by face
    info("Binding faces to game tiles")
    val binding = map.faceRefs.toSeq map { i =>
      debug(s"  Working on face #$i")
      val biome = biomes(i)
      val moisture = moistures.getOrElse(i,0.0) // OCEAN moisture == 0 (as moisture represents drinkable water)
      val resource = biome(rand)
      val cover = coverage(map.convexHull(map.face(i)).toSet)
      val coverByFace = cover map { case (k,(v,_)) => k -> v }
      val prod = production(coverByFace, resource, soils.get(i), conditions.get(i), areas(i), pitches(i))
      val faceAlt = map.vertexProps.getValueOrElse(map.face(i).center, HasForHeight(), 0.0)
      val coverByTile = cover map { case (k,(_,v)) => k -> v }
      val alt  = altitude(coverByTile, faceAlt)
      val biomesCoverages = biomeCover(coverByTile, biome)
      val moistCoverages  = moistureCover(coverByTile, moisture)
      (prod, alt, biomesCoverages, moistCoverages)
    }

    // Aggregate each resource produced by tile location
    info("Processing biomes associated to each tiles")
    val biomesCoverage = binding map { _._3 }
    val aggrBiomes = biomesCoverage.flatten groupBy { _._1 } map { case (k,grouped) =>  k -> (grouped map { _._2 }).toSet }

    info("Processing resources produced by the biomes")
    val productions = binding map { _._1 }
    val aggrProds = productions.flatten groupBy { _._1 } map { case (k,grouped) =>  k -> (grouped map { _._2 })}

    // Assign an altitude to each tile
    info("Processing altitudes associated to tiles")
    val altitudes = binding map { _._2 }
    val aggrAlts = altitudes.flatten groupBy { _._1 } map { case (k, grouped) => k -> (grouped map { _._2}).sum }

    // Processing moistures for each tile
    info("Processing altitudes associated to tiles")
    val moisturesCoverages = binding map { _._4 }
    val aggrMoists = moisturesCoverages.flatten groupBy { _._1 } map { case (k, grouped) =>
      k -> (grouped map { _._2}).sum }

    // Returning the grid
    info("Instantiating the GameBoard")
    val maxIdx = map.size / chunk
    val grid = (for(x <- 0 until maxIdx; y <- 0 until maxIdx) yield (x,y) -> {
      // sum of similar biomes appearing on the very same tile
      val biomes = aggrBiomes((x,y)) groupBy { _._1 } map { case (b, d) => b -> (0.0 /: d ) { (acc,p) => acc + p._2}}
      //Tile(altitude = aggrAlts((x,y)), biomes = biomes.toSet, moisture = aggrMoists((x,y)))
      Tile(altitude = aggrAlts((x,y)), biomes = aggrBiomes((x,y)), moisture = aggrMoists((x,y)))
      }).toMap

    val tiles = (grid /: aggrProds) { case (acc, (loc, stocks)) =>
      val existing = acc(loc)
      acc + (loc -> (existing bulkAdd stocks.toSet))
    }

    info("Introducing Points of Interest")
    val main = GameBoard(map.size, map, tiles, tileUnit = chunk)
    // Specializing the sequence of POIGenerator for this very build
    val gens = poiGenerators map { g => g(rand,locator) _ }
    val withPOIs = (main /: gens) { (acc, gen) => gen(acc) }

    info("GameBoard building process ended")
    withPOIs
  }


  def altitude(cover: Map[(Int, Int), Double], alt: Double): Seq[((Int, Int), Double)] = (cover map {
    case (key, percent) => key -> (alt * (percent / 100))
  }).toSeq

  def biomeCover(cover: Map[(Int, Int), Double], biome: Biome): Seq[((Int, Int), (Biome, Double))] = (cover map {
    case (key, percent) => key -> (biome -> percent / 100)
  }).toSeq

  def moistureCover(cover: Map[(Int, Int), Double], moisture: Double): Seq[((Int, Int), Double)] = (cover map {
    case (key, percent) => key -> (moisture * (percent / 100))
  }).toSeq

  /**
   * Identify the stock to be associated to the set of tiles covered by a given face, based on different parameters.
   *
   * @return the empty sequence if the resource is "None". A sequence of tile location associated to a Stock elsewhere.
   */
  def production(cover: Map[(Int, Int), Double], res: PrimaryResource,
                 soil: Option[Soil], cond: Option[Condition],
                 area: Double, pitch: Double): Seq[((Int, Int), Stock)] = res match {
      case NoResource => Seq()  // producing none means  to disappear
      case r => {
        val amount = res.amount(area, soil, rand)
        val extraction = res.extraction(pitch, cond, rand)
        val dispatch = cover map {
          case ((x, y), percent) => {
            ((x, y), Stock(res, (amount * (percent/100)).ceil.toInt, extraction))
          }
        }
        debug(s"  $res: amount: $amount, Extraction factor: $extraction\n  Dispatch ${dispatch map {
          case (k,s) => k -> s.amount}}")
        dispatch.toSeq
      }
  }

  /**
   * Identify the tiles involved in the bounding box of a given face
   * @param hull the set of points to be analysed
   * @return a set of tile coordinate involved in a square that covers the hull
   */
  def boundingBox(hull: Set[Point]): Set[(Int, Int)] = {
    require(hull.nonEmpty, "The hull cannot be empty")
    val locations = hull map { locator(_) }
    val minX = (locations map { _._1 }).min; val maxX = (locations map { _._1 }).max
    val minY = (locations map { _._2 }).min; val maxY = (locations map { _._2 }).max
    (for(x <- minX to maxX; y <- minY to maxY) yield (x,y)).toSet
  }

  /**
   * For a given face, identify how the face cover each tile.
   *
   * Considering a convex hull ch, an entry like (x,y) -> (f%, t%) in the resulting map means that
   *   - the tile (x,y) contains p % of the area of the polygon modeled by ch.
   *   - the tile (x,y) is covered by the polygon modeled by ch up to t%.
   * @param hull the hull of the face (a set of point)
   * @return a map binding each covered tile to the percentage of coverage for this face.
   */
  def coverage(hull: Set[Point]): Map[(Int,Int), (Double, Double)] = {
    import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
    // building the JTS artifact associated to this face
    val factory = new GeometryFactory()
    val coordinates = hull.toSeq map { p => new Coordinate(p.x, p.y) }
    val convexHull = factory.createPolygon((coordinates :+ coordinates(0)).toArray).convexHull()
    val refSurface = convexHull.getArea
    val tiles = boundingBox(hull)

    // Internal function used to compute the coverage of a tile by the face
    def cover(x: Int, y: Int): ((Int, Int), (Double, Double)) = {
      val minX = (x * chunk).toDouble; val maxX = minX + chunk
      val minY = (y * chunk).toDouble; val maxY = minY + chunk
      val square = factory.createPolygon(Array(new Coordinate(minX, minY), new Coordinate(maxX, minY),
                                               new Coordinate(maxX, maxY), new Coordinate(minX, maxY),
                                               new Coordinate(minX, minY)))
      val intersect = square.intersection(convexHull)
      val area = intersect.getArea
      (x, y) -> (area / refSurface * 100, area / square.getArea * 100)
    }

    // Apply the cover function to each tile, removing the uncovered tile and returning the associated map
    (tiles map { case (x,y) => cover(x, y) } filter { case (_, (value, _)) => value > 0.0 }).toMap
  }
}


/**
 * Class used to locate the tile that contains a given point, in a functional way
 * @param chunk
 */
class TileLocator(chunk: Int) {
  /**
   * locate a point in the board, i.e., find the coordinates of the associated tile in the board
   * @param p the point to locate
   * @return
   */
  def apply(p: Point): (Int, Int) = (p.x.toInt / chunk, p.y.toInt / chunk)

}
