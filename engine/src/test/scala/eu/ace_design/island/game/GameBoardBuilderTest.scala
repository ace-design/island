package eu.ace_design.island.game

import eu.ace_design.island.geom._
import eu.ace_design.island.map._
import eu.ace_design.island.map.resources.{Resource, NoResource}
import eu.ace_design.island.stdlib.{Biomes, Resources}
import eu.ace_design.island.stdlib.POIGenerators.WithCreeks
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameBoardBuilderTest extends SpecificationWithJUnit {

  "GameBoardBuilderTest Specifications".title

  import GameBoardBuilderDataSet.island

  "The GameBoardBuilder" should {

    val m = IslandMap(MeshBuilderTestDataSet.mesh) // m.size == 200
    val builder = new GameBoardBuilder(100) // DEFAULT_TILE_UNIT = 10

    val board = builder(island)
    val f0 = island.convexHull(island.face(0)).toSet
    val f1 = island.convexHull(island.face(1)).toSet
    val f2 = island.convexHull(island.face(2)).toSet
    val f3 = island.convexHull(island.face(3)).toSet

    val epsilon =  0.00001  // The coverage algorithm is "almost" exact (imprecision: 10^-5)

    "reject a chunk size incompatible with the size of the map" in {
      val erroneous = new GameBoardBuilder(chunk = 11)
      erroneous(m) must throwAn[IllegalArgumentException] // 200 % 11 <> 0
    }

    "ensure that the size of the game board is the size of the used map" in {
      board must beAnInstanceOf[GameBoard]
      board.size must_== GameBoardBuilderDataSet.island.size
    }

    "identify in which tile is located a given point" in {
      val locator = builder.locator
      locator(Point(0.0, 0.0))    must_==(0, 0)
      locator(Point(99.0, 188.0)) must_==(0, 1)      // TILE_UNIT = 100
    }

    "identify the bounding box of a given face" in {
      builder.boundingBox(f0) must_== Set((0, 0), (1, 0), (2, 0), (3, 0),
                                          (0, 1), (1, 1), (2, 1), (3, 1))
      builder.boundingBox(f1) must_== Set((0, 0), (1, 0), (2, 0),
                                          (0, 1), (1, 1), (2, 1),
                                          (0, 2), (1, 2), (2, 2),
                                          (0, 3), (1, 3), (2, 3))
      builder.boundingBox(f2) must_== Set((0, 1), (1, 1), (2, 1), (3, 1),
                                          (0, 2), (1, 2), (2, 2), (3, 2),
                                          (0, 3), (1, 3), (2, 3), (3, 3))
      builder.boundingBox(f3) must_== Set((2, 0), (3,0),
                                          (2, 1), (3,1),
                                          (2, 2), (3,2),
                                          (2, 3), (3,3))
    }

    "identify the tiles covered by a given face" in {
      val c0 = builder.coverage(f0)
      (c0 map { case (k,(v,_)) => k -> v }).values.sum must beCloseTo(100.0, epsilon)
      c0.keys must_== Set((0,0), (1,0), (2,0))
      c0((0,0))._1 must beCloseTo(25.0/150*100, epsilon); c0((0,0))._2 must beCloseTo(25.0, epsilon)
      c0((1,0))._1 must beCloseTo(75.0/150*100, epsilon); c0((1,0))._2 must beCloseTo(75.0, epsilon)
      c0((2,0))._1 must beCloseTo(50.0/150*100, epsilon); c0((2,0))._2 must beCloseTo(50.0, epsilon)

      val c1 = builder.coverage(f1)
      (c1 map { case (k,(v,_)) => k -> v }).values.sum must beCloseTo(100.0, 0.0001)
      c1.keys must_== Set((0,0), (1,0), (0,1), (1,1), (0,2))
      c1((0,0))._1 must beCloseTo(75.0/300*100, epsilon); c1((0,0))._2 must beCloseTo(75.0, epsilon)
      c1((1,0))._1 must beCloseTo(25.0/300*100, epsilon); c1((1,0))._2 must beCloseTo(25.0, epsilon)
      c1((0,1))._1 must beCloseTo(100.0/300*100,epsilon); c1((0,1))._2 must beCloseTo(100.0,epsilon)
      c1((1,1))._1 must beCloseTo(50.0/300*100, epsilon); c1((1,1))._2 must beCloseTo(50.0, epsilon)
      c1((0,2))._1 must beCloseTo(50.0/300*100, epsilon); c1((0,2))._2 must beCloseTo(50.0, epsilon)

      val c2 = builder.coverage(f2)
      (c2 map { case (k,(v,_)) => k -> v }).values.sum must beCloseTo(100.0, 0.0001)
      c2.keys must_== Set((0,2), (1,2), (2,2), (1,1), (2,1))
      c2((0,2))._1 must beCloseTo(50.0/300*100, epsilon); c2((0,2))._2 must beCloseTo(50.0, epsilon)
      c2((1,2))._1 must beCloseTo(100.0/300*100,epsilon); c2((1,2))._2 must beCloseTo(100.0,epsilon)
      c2((2,2))._1 must beCloseTo(75.0/300*100, epsilon); c2((2,2))._2 must beCloseTo(75.0, epsilon)
      c2((1,1))._1 must beCloseTo(50.0/300*100, epsilon); c2((1,1))._2 must beCloseTo(50.0, epsilon)
      c2((2,1))._1 must beCloseTo(25.0/300*100, epsilon); c2((2,1))._2 must beCloseTo(25.0, epsilon)

      val c3 = builder.coverage(f3)
      (c3 map { case (k,(v,_)) => k -> v }).values.sum must beCloseTo(100.0, 0.0001)
      c3.keys must_== Set((2,0), (2,1), (2,2))
      c3((2,0))._1 must beCloseTo(50.0/150*100, epsilon); c3((2,0))._2 must beCloseTo(50.0, epsilon)
      c3((2,1))._1 must beCloseTo(75.0/150*100, epsilon); c3((2,1))._2 must beCloseTo(75.0, epsilon)
      c3((2,2))._1 must beCloseTo(25.0/150*100, epsilon); c3((2,2))._2 must beCloseTo(25.0, epsilon)
    }

    "build a game board with tiles consistent with the input island" in {
      board.tiles.keys.toSet must_== GameBoardBuilderDataSet.tiles
    }

    "Identify the resources associated to a given face" in {
      import Resources._
      import eu.ace_design.island.map.resources.Soils._
      import eu.ace_design.island.map.resources.Conditions._

      val c0 = builder.coverage(f0) map { case (k,(v,_)) => k -> v }
      val p0 = builder.production(c0, WOOD, Some(NORMAL), Some(FAIR), 150.0, 0.0).toMap
      p0.keys must_== Set((0, 0), (1, 0), (2, 0))
      (p0.values map { _.resource }).toSet must_== Set(WOOD)

      val c1 = builder.coverage(f1) map { case (k,(v,_)) => k -> v }
      val p1 = builder.production(c1, ORE, Some(POOR), Some(FAIR), 300.0, 0.0).toMap
      p1.keys must_== Set((0, 0), (0, 1), (0, 2), (1, 0), (1, 1))
      (p1.values map { _.resource }).toSet must_== Set(ORE)

      val c2 = builder.coverage(f2) map { case (k,(v,_)) => k -> v }
      val p2 = builder.production(c2, FLOWER, Some(FERTILE), Some(HARSH), 300.0, 0.0).toMap
      p2.keys must_== Set((0, 2), (1, 2), (2, 2), (1, 1), (2, 1))
      (p2.values map { _.resource }).toSet must_== Set(FLOWER)

      val c3 = builder.coverage(f3) map { case (k,(v,_)) => k -> v }
      val p3 = builder.production(c3, NoResource, Some(POOR), Some(EASY), 300.0, 0.0).toMap
      p3.keys must_== Set()
      (p3.values map { _.resource }).toSet must_== Set()

    }

    "Assign relevant resources to each tile" in {
      import Resources._
      def oracle(x: Int, y: Int): Set[Set[Resource]] = (x, y) match {
        case (0, 0) => Set(Set(WOOD, FUR))
        case (1, 0) => Set(Set(WOOD, FUR))
        case (2, 0) => Set(Set(WOOD, FLOWER), Set(WOOD))
        case (0, 1) => Set(Set(FUR))
        case (1, 1) => Set(Set(FUR, WOOD), Set(FUR, FLOWER))
        case (2, 1) => Set(Set(WOOD, FLOWER), Set(FLOWER), Set(WOOD))
        case (0, 2) => Set(Set(FUR, WOOD), Set(FUR, FLOWER))
        case (1, 2) => Set(Set(WOOD), Set(FLOWER))
        case (2, 2) => Set(Set(WOOD, FLOWER), Set(FLOWER), Set(WOOD))
        case _ => throw new IllegalArgumentException()
      }
      def assess(x: Int, y: Int) = {
        val expected = oracle(x, y)
        val actual = board.produces(x, y)
        //println(s"($x,$y) / expected: $expected / actual: $actual")
        expected.intersect(Set(actual)) must not(beEmpty)
      }
      assess(0, 0)
      assess(0, 1)
      assess(0, 2)
      assess(1, 0)
      assess(1, 1)
      assess(1, 2)
      assess(2, 0)
      assess(2, 1)
      assess(2, 2)
    }

    "Assign relevant biomes to each tile" in {
      import Biomes._
      val (b0, b1, b2, b3) = (TEMPERATE_DECIDUOUS_FOREST, TUNDRA, MANGROVE, GLACIER)
      board.at(0,0).biomes map { _._1 } must_== Set(b0,b1)
      board.at(0,0).biomes foreach { case (b,v) =>
        if (b == b0) (v must beCloseTo(25, epsilon)) else (v must beCloseTo(75, epsilon)) }
      board.at(0,1).biomes map { _._1 } must_== Set(b1)
      board.at(0,1).biomes.size must_== 1
      board.at(0,1).biomes.head._2 must beCloseTo (100, epsilon)
      // TODO: continue tile coverage, but should be OK as Altitude is already ok and use similar mechanisms
    }


    "assign relevant altitudes to each tile" in {
      val (a0, a1, a2, a3) = (100.0, 300.0, 200.0, 400.0)
      board.at(0,0).altitude must beCloseTo(0.75*a1 + 0.25*a0, epsilon)
      board.at(0,1).altitude must beCloseTo(a1,                epsilon)
      board.at(0,2).altitude must beCloseTo(0.50*a1 + 0.50*a2, epsilon)
      board.at(1,0).altitude must beCloseTo(0.25*a1 + 0.75*a0, epsilon)
      board.at(1,1).altitude must beCloseTo(0.50*a1 + 0.50*a2, epsilon)
      board.at(1,2).altitude must beCloseTo(a2,                epsilon)
      board.at(2,0).altitude must beCloseTo(0.50*a0 + 0.50*a3, epsilon)
      board.at(2,1).altitude must beCloseTo(0.75*a3 + 0.25*a2, epsilon)
      board.at(2,2).altitude must beCloseTo(0.75*a2 + 0.25*a3, epsilon)
    }

    "assign relevant moistures to each tile" in {
      val (m0, m1, m2, m3) = (100.0, 300.0, 200.0, 400.0)
      board.at(0,0).moisture must beCloseTo(0.75*m1 + 0.25*m0, epsilon)
      board.at(0,1).moisture must beCloseTo(m1,                epsilon)
      board.at(0,2).moisture must beCloseTo(0.50*m1 + 0.50*m2, epsilon)
      board.at(1,0).moisture must beCloseTo(0.25*m1 + 0.75*m0, epsilon)
      board.at(1,1).moisture must beCloseTo(0.50*m1 + 0.50*m2, epsilon)
      board.at(1,2).moisture must beCloseTo(m2,                epsilon)
      board.at(2,0).moisture must beCloseTo(0.50*m0 + 0.50*m3, epsilon)
      board.at(2,1).moisture must beCloseTo(0.75*m3 + 0.25*m2, epsilon)
      board.at(2,2).moisture must beCloseTo(0.75*m2 + 0.25*m3, epsilon)
    }

    "by default do not add any POIs to the board" in {
      board.pois must beEmpty
    }

    "execute a sequence of POIs generator when provided" in {
      val boardPrime = (new GameBoardBuilder(100, Seq(new WithCreeks(howMany = 10))))(island)
      boardPrime.pois.values.flatten must haveSize(10)
    }

  }
}

object GameBoardBuilderDataSet {
  import eu.ace_design.island.map.{HasForBiome, HasForArea, HasForCondition, HasForSoil, PropertySet, IslandMap}
  import Biomes._
  import eu.ace_design.island.map.resources.Conditions._
  import eu.ace_design.island.map.resources.Soils._
  /**
   * Minimal representation of an island to be used for test purpose
   *
   *                     Vertices:               Edges:
   *   0              1    - p0 = (0 ,  0 )         - e0 = p0 -- p1     - e4 = p3 -- p0
   *   X----+----+----X    - p1 = (300, 0 )         - e1 = p1 -- p2     - e5 = p3 -- p4
   *   |    |    |    |    - p2 = (200, 100)        - e2 = p2 -- p0     - e6 = p4 -- p2
   *   |    |  2 |    |    - p3 = (0,   300)        - e3 = p2 -- p3     - e7 = p4 -- p1
   *   +----+----X----+    - p4 = (300, 300)
   *   |    |    |    |
   *   |    |    |    |  Faces: (geometry, Biome, soil \in {FERTILE, NORMAL, POOR}, conditions \in {EASY, FAIR, HARSH})
   *   +----+----+----+    - f0 = {e0, e1, e2} = (0,1,2), center p5: TEMPERATE_DECIDUOUS_FOREST, NORMAL, FAIR
   *   |    |    |    |    - f1 = {e2, e3, e4} = (0,2,3), center p6: TUNDRA, POOR, FAIR
   *   |    |    |    |    - f2 = {e3, e5, e6} = (3,2,4), center p7: MANGROVE, FERTILE, HARSH
   *   X----+----+----X    - f3 = {e6, e1, e7} = (4,2,1), center p8: GLACIER, POOR, EASY
   *   3              4
   *                     Resources: (see eu.ace_design.island.map.resources.BiomeToResources for ratios)
   *  0           1        - TEMPERATE_DECIDUOUS_FOREST produces WOOD (100%)
   *   X----------X        - TUNDRA                     produces FUR  (100%)
   *   |  \ 2   / |        - MANGROVE                   produces WOOD (60%)  or FLOWER (40%)
   *   |      X   |        - GLACIER                    produces FLOWER (5%) or None   (95%)
   *   |    /  \  |
   *   |  /     \ |      Areas:            Total = 90,000 px2       Altitudes:
   *   |/        \|        - area(f0) = area(f3) = 15,000 px2         f0: 100     f1: 300
   *   X----------X        - area(f1) = area(f2) = 30,000 px2         f2: 200     f3: 400
   *   3          4
   **/

  private def centroid(pi: Int, pj: Int, pk: Int): Point = 
    Point((vertices(pi).x + vertices(pj).x + vertices(pk).x)/3, (vertices(pi).y + vertices(pj).y + vertices(pk).y)/3)
  
  val vertices = Seq(Point(0.0, 0.0), Point(300.0, 0.0), Point(200.0, 100.0), Point(0.0, 300.0), Point(300.0, 300.0))
  
  val vReg = (VertexRegistry() /: vertices) { (acc, point) => acc + point } +
              centroid(0,1,2) + centroid(0,2,3) + centroid(3,2,4) + centroid(4,2,1)
  val eReg = EdgeRegistry() + Edge(0,1) + Edge(1,2) + Edge(2,0) + Edge(2,3) +
                              Edge(3,0) + Edge(3,4) + Edge(4,2) + Edge(4,1)
  // We do not exploit neighborhood relationship between faces to build the board => set to None (default value)
  val fReg = FaceRegistry() + Face(5, Seq(0,1,2)) + Face(6, Seq(2,3,4)) + Face(7, Seq(3,5,6)) + Face(8, Seq(1,6,7))

  val vProps = PropertySet() + (5 -> HasForHeight(100.0)) + (6 -> HasForHeight(300.0)) + 
                                      (7 -> HasForHeight(200.0)) + (8 -> HasForHeight(400.0)) +
                               (0 -> IsCoast()) + (2 -> IsCoast())
  
  val fProps = PropertySet() +
                    // Face 0: TEMPERATE_DECIDUOUS_FOREST, NORMAL, FAIR
                    (0 -> HasForBiome(TEMPERATE_DECIDUOUS_FOREST)) + (0 -> HasForArea(15000.0)) + (0 -> HasForPitch(0.0)) +
                        (0 -> HasForSoil(NORMAL))  + (0 -> HasForCondition(FAIR))  + (0 -> HasForMoisture(100.0)) +
                    // Face 1: TUNDRA, POOR, FAIR
                    (1 -> HasForBiome(TUNDRA))           + (1 -> HasForArea(30000.0)) + (1 -> HasForPitch(0.0)) +
                        (1 -> HasForSoil(POOR))    + (1 -> HasForCondition(FAIR))  +  (1 -> HasForMoisture(300.0)) +
                    // Face 2: MANGROVE, FERTILE, HARSH
                    (2 -> HasForBiome(MANGROVE))         + (2 -> HasForArea(30000.0)) + (2 -> HasForPitch(0.0)) +
                        (2 -> HasForSoil(FERTILE)) + (2 -> HasForCondition(HARSH)) +  (2 -> HasForMoisture(200.0))  +
                    // Face 3: GLACIER, POOR, EASY
                    (3 -> HasForBiome(GLACIER))          + (3 -> HasForArea(15000.0)) + (3 -> HasForPitch(0.0)) +
                        (3 -> HasForSoil(POOR))    + (3 -> HasForCondition(EASY)) + (3 -> HasForMoisture(400.0))

  val island = IslandMap(Mesh(vReg, eReg, fReg, Some(300))).copy(vertexProps = vProps, faceProps = fProps)

  val tiles = Set( (0,0), (0,1), (0,2),  (1,0), (1,1), (1,2),  (2,0), (2,1), (2,2) )
}

