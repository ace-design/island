package eu.ace_design.island.game

import eu.ace_design.island.geom._
import eu.ace_design.island.map.IslandMap
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameBoardBuilderTest extends SpecificationWithJUnit {

  "GameBoardBuilderTest Specifications".title

  "The GameBoardBuilder" should {

    val m = IslandMap(MeshBuilderTestDataSet.mesh) // m.size == 200
    val builder = new GameBoardBuilder()   // chunk = DEFAULT_TILE_UNIT = 10
    val triangle = Set(Point(5.0, 6.0), Point(18.9, 28.3), Point(26.4, 15.5))
    val board = builder(GameBoardBuilderDataSet.island)

    "reject a chunk size incompatible with the size of the map" in {
      val erroneous = new GameBoardBuilder(chunk = 11)
      erroneous(m) must throwAn[IllegalArgumentException]  // 200 % 11 <> 0
    }

    "ensure that the size of the game board is the size of the used map" in {
      board must beAnInstanceOf[GameBoard]
      board.size must_== GameBoardBuilderDataSet.island.size
    }

    "identify in which tile is located a given point" in {
      builder.locate(Point(0.0,0.0))     must_== (0,0)
      builder.locate(Point(99.0, 188.0)) must_== (9, 18)
    }

    "identify the bounding box of a given face" in {
      // triangle(0) \in (0,0), triangle(1) \in (1,2) and triangle(2) \in (2,1)
      builder.boundingBox(triangle) must_== GameBoardBuilderDataSet.tiles
    }

    "identify the tiles covered by a given face" in {
      val coverage = builder.coverage(triangle)
      coverage.values.sum must beCloseTo(100.0, 0.0001) // The coverage algorithm is "almost" exact
      // The triangle is not located on the upper right (0,2) and lower left (2,0) of its bounding box
      coverage.keys.toSet must_== Set((0,0), (0,1), (1,0), (1,1), (1,2), (2,1), (2,2))
    }

    "build a game board of size 3 x 3 using the example island" in {
      board.tiles.keys.toSet must_== GameBoardBuilderDataSet.tiles
    }


    "Identify the resources associated to a given face" in {
      import eu.ace_design.island.map.resources.ExistingResources._
      import eu.ace_design.island.map.resources.Soils._
      import eu.ace_design.island.map.resources.Conditions._

      val f0 = Set(Point(0.0,0.0), Point(30.0, 0.0), Point(20.0, 10.0))
      val p0 = builder.production(f0, WOOD, NORMAL, FAIR, 150.0).toMap
      p0.keys must_== Set((0,0), (1,0), (2,0))
      (p0.values map { _.resource }).toSet must_== Set(WOOD)

      val f1 = Set(Point(0.0,0.0), Point(0.0, 30.0), Point(20.0, 10.0))
      val p1 = builder.production(f1, ORE, POOR, FAIR, 300.0).toMap
      p1.keys must_== Set((0,0), (0,1), (0,2), (1,0), (1,1))
      (p1.values map { _.resource }).toSet must_== Set(ORE)

      val f2 = Set(Point(30.0,30.0), Point(0.0, 30.0), Point(20.0, 10.0))
      val p2 = builder.production(f2, FLOWER, FERTILE, HARSH, 300.0).toMap
      p2.keys must_== Set((0,2), (1,2), (2,2), (1,1), (2,1))
      (p2.values map { _.resource }).toSet must_== Set(FLOWER)

      val f3 = Set(Point(30.0,30.0), Point(30.0, 0.0), Point(20.0, 10.0))
      val p3 = builder.production(f3, None, POOR, EASY, 300.0).toMap
      p3.keys must_== Set()
      (p3.values map { _.resource }).toSet must_== Set()

    }

    "Assign relevant resources to each tile" in {
      import eu.ace_design.island.map.resources.ExistingResources._
      def oracle(x: Int, y: Int): Set[Set[Resource]] = (x,y) match {
        case (0,0) => Set(Set(WOOD, ORE))
        case (1,0) => Set(Set(WOOD, ORE))
        case (2,0) => Set(Set(WOOD, FLOWER), Set(WOOD))
        case (0,1) => Set(Set(ORE))
        case (1,1) => Set(Set(ORE, WOOD), Set(ORE, FLOWER))
        case (2,1) => Set(Set(WOOD, FLOWER), Set(FLOWER))
        case (0,2) => Set(Set(ORE, WOOD), Set(ORE, FLOWER))
        case (1,2) => Set(Set(WOOD), Set(FLOWER))
        case (2,2) => Set(Set(WOOD, FLOWER), Set(FLOWER))
        case _ => throw new IllegalArgumentException()
      }
      board.tiles.keys foreach { case (x,y) =>
        val expected = oracle(x,y)
        val actual = board.produces(x,y)
        println(s"($x,$y) / expected: $expected / actual: $actual")
        expected.intersect(Set(actual)) must not(beEmpty)
      }
      true must beTrue
    }


  }
}

object GameBoardBuilderDataSet {
  import eu.ace_design.island.map.{HasForBiome, HasForArea, HasForCondition, HasForSoil, PropertySet, IslandMap}
  import eu.ace_design.island.map.resources.ExistingBiomes._
  import eu.ace_design.island.map.resources.Conditions._
  import eu.ace_design.island.map.resources.Soils._
  /**
   * Minimal representation of an island to be used for test purpose
   *
   *                     Vertices:               Edges:
   *   0              1    - p0 = (0 , 0 )         - e0 = p0 -- p1     - e4 = p3 -- p0
   *   X----+----+----X    - p1 = (30, 0 )         - e1 = p1 -- p2     - e5 = p3 -- p4
   *   |    |    |    |    - p2 = (20, 10)         - e2 = p2 -- p0     - e6 = p4 -- p2
   *   |    |  2 |    |    - p3 = (0,  30)         - e3 = p2 -- p3     - e7 = p4 -- p1
   *   +----+----X----+    - p4 = (30, 30)
   *   |    |    |    |
   *   |    |    |    |  Faces: (geometry, Biome, soil \in {FERTILE, NORMAL, POOR}, conditions \in {EASY, FAIR, HARSH})
   *   +----+----+----+    - f0 = {e0, e1, e2} = (0,1,2), center p5: TEMPERATE_DECIDUOUS_FOREST, NORMAL, FAIR
   *   |    |    |    |    - f1 = {e2, e3, e4} = (0,2,3), center p6: TEMPERATE_DESERT, POOR, FAIR
   *   |    |    |    |    - f2 = {e3, e5, e6} = (3,2,4), center p7: MANGROVE, FERTILE, HARSH
   *   X----+----+----X    - f3 = {e6, e1, e7} = (4,2,1), center p8: GLACIER, POOR, EASY
   *   3              4
   *                     Resources: (see eu.ace_design.island.map.resources.BiomeToResources for ratios)
   *  0           1        - TEMPERATE_DECIDUOUS_FOREST produces WOOD (100%)
   *   X----------X        - TEMPERATE_DESERT           produces ORE  (100%)
   *   |  \ 2   / |        - MANGROVE                   produces WOOD (60%)  or FLOWER (40%)
   *   |      X   |        - GLACIER                    produces FLOWER (5%) or None   (95%)
   *   |    /  \  |
   *   |  /     \ |      Areas:
   *   |/        \|        - area(f0) = area(f3) = 150 px2
   *   X----------X        = area(f1) = area(f2) = 300 px2
   *   3          4
   **/

  private def centroid(pi: Int, pj: Int, pk: Int): Point = 
    Point((vertices(pi).x + vertices(pj).x + vertices(pk).x)/3, (vertices(pi).y + vertices(pj).y + vertices(pk).y)/3)
  
  val vertices = Seq(Point(0.0, 0.0), Point(30.0, 0.0), Point(20.0, 10.0), Point(0.0, 30.0), Point(30.0, 30.0))
  
  val vReg = (VertexRegistry() /: vertices) { (acc, point) => acc + point } +
              centroid(0,1,2) + centroid(0,2,3) + centroid(3,2,4) + centroid(4,2,1)
  val eReg = EdgeRegistry() + Edge(0,1) + Edge(1,2) + Edge(2,0) + Edge(2,3) +
                              Edge(3,0) + Edge(3,4) + Edge(4,2) + Edge(4,1)
  // We do not exploit neighborhood relationship between faces to build the board => set to None (default value)
  val fReg = FaceRegistry() + Face(5,Seq(0,1,2)) + Face(6, Seq(0,2,3)) + Face(7, Seq(3,2,4)) + Face(8, Seq(4,2,1))

  val properties = PropertySet() +
                    // Face 0: TEMPERATE_DECIDUOUS_FOREST, NORMAL, FAIR
                    (0 -> HasForBiome(TEMPERATE_DECIDUOUS_FOREST)) + (0 -> HasForArea(150.0)) +
                        (0 -> HasForSoil(NORMAL)) + (0 -> HasForCondition(FAIR)) +
                    // Face 1: TEMPERATE_DESERT, POOR, FAIR
                    (1 -> HasForBiome(TEMPERATE_DESERT))           + (1 -> HasForArea(300.0)) +
                        (1 -> HasForSoil(POOR))   + (1 -> HasForCondition(FAIR)) +
                    // Face 2: MANGROVE, FERTILE, HARSH
                    (2 -> HasForBiome(MANGROVE))                   + (2 -> HasForArea(300.0)) +
                        (2 -> HasForSoil(FERTILE)) + (2 -> HasForCondition(HARSH)) +
                    // Face 3: GLACIER, POOR, EASY
                    (3 -> HasForBiome(GLACIER)) + (3 -> HasForArea(150.0)) +
                        (3 -> HasForSoil(POOR)) + (3 -> HasForCondition(EASY))

  val island = IslandMap(Mesh(vReg, eReg, fReg, Some(30))).copy(faceProps = properties)

  val tiles = Set( (0,0), (0,1), (0,2),  (1,0), (1,1), (1,2),  (2,0), (2,1), (2,2) )
}

