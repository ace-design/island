package eu.ace_design.island.game

import eu.ace_design.island.geom.Point
import eu.ace_design.island.map._
import eu.ace_design.island.map.resources.Soils.Soil
import eu.ace_design.island.map.resources.Conditions.Condition
import eu.ace_design.island.map.resources.ExistingResources.Resource
import eu.ace_design.island.map.resources.{ExistingResources, BiomeToResource}
import scala.util.Random


/**
 * This class is used to bind a Generated map to a GameBoard
 *
 * Please do look at the commit date before complaining about the code quality ^^
 *
 * @param chunk the size of each tile (map.size must be a factor of chunk when applied)
 * @param rand a random generator, to be forwarded to the Biome2Resource mapper
 */
class GameBoardBuilder(chunk: Int = DEFAULT_TILE_UNIT, rand: Random = new Random()) {

  /**
   * Build a game board on top of a given map
   * @param map the map to use as input
   * @return the associated game board
   */
  def apply(map: IslandMap): GameBoard = {                               
    require(map.size % chunk == 0, "the size of the island must be compatible with the size of a tile")
    // Extracting relevant information from the map
    val biomes     = map.faceProps.restrictedTo(HasForBiome())
    val soils      = map.faceProps.restrictedTo(HasForSoil())
    val conditions = map.faceProps.restrictedTo(HasForCondition())
    val areas      = map.faceProps.restrictedTo(HasForArea())
    // Computing the resources associated to tile, face by face
    val productions = map.faceRefs.toSeq map { i =>
      val resource = BiomeToResource(biomes(i), rand)
      production(map.convexHull(map.face(i)).toSet, resource, soils(i), conditions(i), areas(i))
    }
    // Aggregate each resource produced by tile location
    val aggregated = productions.flatten groupBy { _._1 } map { case (k,grouped) =>  k -> (grouped map { _._2 })}
    //val asTiles = aggregated map { case ((x,y),stocks) => (x,y) -> Tile(stocks.toSet) }
    // Returning the grid
    val maxIdx = map.size / chunk
    val grid = (for(x <- 0 until maxIdx; y <- 0 until maxIdx) yield (x,y) -> Tile()).toMap
    val tiles = (grid /: aggregated) { case (acc, (loc, stocks)) =>
      val existing = acc(loc)
      acc + (loc -> (existing ++ stocks.toSet))
    }
    GameBoard(map.size, tiles)
  }

  /**
   * Identify the stock to be associated to the set of tiles covered by a given face, based on different parameters.
   *
   * @return the empty sequence if the resource is "None". A sequence of tile location associated to a Stock elsewhere.
   */
  def production(hull: Set[Point], res: Resource, soil: Soil, cond: Condition, area: Double): Seq[((Int, Int), Stock)] = {
    res match {
      case ExistingResources.None => Seq()  // producing none means  to disappear
      case r => (coverage(hull) map { case ((x, y), percent) => ((x, y), Stock(res, 100))}).toSeq
    }
  }

  /**
   * locate a point in the board, i.e., find the coordinates of the associated tile in the board
   * @param p the point to locate
   * @return
   */
  def locate(p: Point): (Int, Int) = (p.x.toInt / chunk, p.y.toInt / chunk)

  /**
   * Identify the tiles involved in the bounding box of a given face
   * @param hull the set of points to be analysed
   * @return a set of tile coordinate involved in a square that covers the hull
   */
  def boundingBox(hull: Set[Point]): Set[(Int, Int)] = {
    require(hull.nonEmpty, "The hull cannot be empty")
    val locations = hull map { locate }
    val minX = (locations map { _._1 }).min; val maxX = (locations map { _._1 }).max
    val minY = (locations map { _._2 }).min; val maxY = (locations map { _._2 }).max
    (for(x <- minX to maxX; y <- minY to maxY) yield (x,y)).toSet
  }

  /**
   * for a given face, identify the percentage of a tile covered by the face.
   * @param hull the hull of the face (a set of point)
   * @return a map binding each covered tile to the percentage of coverage for this face
   */
  def coverage(hull: Set[Point]): Map[(Int,Int), Double] = {
    import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
    // building the JTS artifact associated to this face
    val factory = new GeometryFactory()
    val coordinates = hull.toSeq map { p => new Coordinate(p.x, p.y) }
    val convexHull = factory.createPolygon((coordinates :+ coordinates(0)).toArray).convexHull()
    val refSurface = convexHull.getArea
    val tiles = boundingBox(hull)

    // Internal function used to compute the coverage of a tile by the face
    def cover(x: Int, y: Int): ((Int, Int), Double) = {
      val minX = (x * chunk).toDouble; val maxX = minX + chunk
      val minY = (y * chunk).toDouble; val maxY = minY + chunk
      val square = factory.createPolygon(Array(new Coordinate(minX, minY), new Coordinate(maxX, minY),
                                               new Coordinate(maxX, maxY), new Coordinate(minX, maxY),
                                               new Coordinate(minX, minY)))
      val intersect = square.intersection(convexHull)
      (x, y) -> (intersect.getArea / refSurface * 100)
    }

    // Apply the cover function to each tile, removing the uncovered tile and returning the associated map
    (tiles map { case (x,y) => cover(x, y) } filter { case (_, value) => value > 0.0 }).toMap
  }


}
