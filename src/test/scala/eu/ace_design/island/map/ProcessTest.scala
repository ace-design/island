package eu.ace_design.island.map

import eu.ace_design.island.geom.{MeshBuilder, SquaredGrid, Point}

import eu.ace_design.island.map.processes._
import org.specs2.mutable._



class ProcessTest extends SpecificationWithJUnit {
  final val SIZE = 600
  final val FACES = 100

  val generator = new SquaredGrid(SIZE)
  // A rectangular grid, 600 x 600.
  val builder = new MeshBuilder(SIZE)
  // A mesh builder for 600 x 600 grids
  val mesh = builder(generator(FACES))
  // generating the mesh, with 100 faces involved in the grid
  val entry = IslandMap(mesh)

  "The IdentifyBorders process" should {
    val updated = IdentifyBorders(entry)  // no pre-conditions
    val borderPoints = updated.vertexProps.project(updated.mesh.vertices)(Set(IsBorder()))
    val borderFaces  = updated.faceProps.project(updated.mesh.faces)(Set(IsBorder()))

    "annotate with IsBorder the faces that touch the external boundary" in {
      // This trick only work because we are using a grid-based generator
      updated.faceProps.size must_== 4 * (math.sqrt(FACES).toInt - 1)
    }
    "annotate as border the points that touch the edge of the map" in {
      val check = (p: Point) => p.x == 0 || p.x == updated.mesh.size.get || p.y == 0 || p.y == updated.mesh.size.get
      borderPoints foreach { check(_) must beTrue }
      updated.vertexProps.size must beGreaterThan(0)
    }
    "leave faces that are not border one unchanged" in {
      val refs = borderFaces map { updated.mesh.faces(_).get }
      val otherFaces = updated.mesh.faces.references diff refs
      otherFaces foreach { ref => updated.faceProps.get(ref) must_== entry.faceProps.get(ref) }
      true must beTrue // to return a spec fragment and thus allow compilation.
    }
    "leave vertices that are not borders unchanged" in {
      val refs = borderPoints map { updated.mesh.vertices(_).get }
      val otherPoints = updated.mesh.vertices.references diff refs
      otherPoints foreach { ref => updated.vertexProps.get(ref) must_== entry.vertexProps.get(ref) }
      true must beTrue // to return a spec fragment and thus allow compilation.
    }
    "automatically consider border elements (faces and vertices) as water ones" in {
      val faceRefs = borderFaces map { updated.mesh.faces(_).get }
      val verticesRefs = borderPoints map { updated.mesh.vertices(_).get }
      faceRefs foreach { updated.faceProps.check(_, IsWater()) must beTrue }
      verticesRefs foreach { updated.vertexProps.check(_, IsWater()) must beTrue }
      true must beTrue
    }
  }

  "The IdentifyWaterArea process" should {
    val process = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8), threshold = 30)
    val updated = process(entry)   // no pre-conditions

    "annotate all the faces with IsWater properties" in {
      val props = updated.faceProps.project(updated.mesh.faces) _
      val waters = props(Set(IsWater()))
      val lands = props(Set(!IsWater()))
      waters ++ lands must_== mesh.faces.values
    }
  }

  "The AlignVertexWaterBasedOnFaces process" should {
    val precondition = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8), threshold = 30)
    val updated = AlignVertexWaterBasedOnFaces(precondition(entry))
    val fProps = updated.faceProps.project(updated.mesh.faces) _
    val vProps = updated.vertexProps.project(updated.mesh.vertices) _
    "consider vertices involved in land faces as land" in {
      val vertices = fProps(Set(!IsWater())) flatMap { _.vertices(updated.mesh.edges) }
      vertices map { updated.vertexProps.check(_,!IsWater()) } must contain(beTrue)
    }
    "align faces' center to their associated face" in {
      val faces = fProps(Set(!IsWater()))
      faces foreach { f =>
        val ref = updated.mesh.faces(f).get
        val faceVal = updated.faceProps.getValue(ref,IsWater())
        updated.vertexProps.check(f.center, IsWater(faceVal)) must beTrue
      }
      true must beTrue
    }
    "tag all vertices defined in the map" in {
      val vertices = updated.mesh.vertices.values
      val landVertices  = vProps(Set(!IsWater()))
      val waterVertices = vProps(Set(IsWater()))
      (landVertices ++ waterVertices) must_== vertices
    }

  }

  "The IdentifyLakesAndOcean process" should {
    import eu.ace_design.island.map.ExistingWaterKind._
    // preconditions:  only works on faces => neglect AlignVertex...  Requires borders and IdentifyWaterArea
    val preconditions : IslandMap => IslandMap = { m =>
      val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
      IdentifyWaterArea(shape = donuts, threshold = 30)(IdentifyBorders(m))
    }
    val updated = IdentifyLakesAndOcean(preconditions(entry))
    val props = updated.faceProps.project(updated.mesh.faces) _
    val waters = props(Set(IsWater()))
    val oceans = props(Set(WaterKind(OCEAN)))
    val lakes = props(Set(WaterKind(LAKE)))

    "annotate all the water faces with WaterKind properties" in { lakes ++ oceans must_== waters }
    "identify the external ocean" in { oceans.size must be greaterThan 0 }
    "identify the central lake of this donuts-shaped island" in { lakes.size  must be greaterThan 0 }
  }

  "The IdentifyCoastLine process" should {
    val preconditions : IslandMap => IslandMap = { m =>
      val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
      IdentifyLakesAndOcean(AlignVertexWaterBasedOnFaces(IdentifyWaterArea(donuts,30)(IdentifyBorders(m))))
    }
    val updated = IdentifyCoastLine(preconditions(entry))

    "annotate land faces with an IsCoast tag" in {
      val finder = updated.faceProps.project(updated.mesh.faces) _
      val land = finder(Set(!IsWater()))
      val coast = finder(Set(IsCoast()))
      coast must not(beEmpty)
      (coast & land) must_== coast
    }
    "Annotate land vertices with the IsCoast tag" in {
      val finder = updated.vertexProps.project(updated.mesh.vertices) _
      val coast = finder(Set(IsCoast()))
      val land = finder(Set(!IsWater()))
      coast must not(beEmpty)
      (coast & land) must_== coast
    }
  }

  "The DistanceToCoast process" should {
    val preconditions: IslandMap => IslandMap = { m =>
      val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
      IdentifyCoastLine(IdentifyLakesAndOcean(AlignVertexWaterBasedOnFaces(IdentifyWaterArea(donuts, 30)(IdentifyBorders(m)))))
    }
    val updated = MinimalDistanceToCoast(preconditions(entry))
    val props = updated.vertexProps.project(updated.mesh.vertices) _
    "consider coastal vertices as lowest distance (0)" in {
      val coast = props(Set(IsCoast())) map {
        updated.mesh.vertices(_).get
      }
      coast foreach {
        updated.vertexProps.getValue(_, DistanceToCoast()) must_== 0
      }
      true must beTrue
    }
    "computes relative distances (in [0,1])" in {
      val land = props(Set(!IsWater())) map { updated.mesh.vertices(_).get  }
      val distances = land map { updated.vertexProps.getValue(_, DistanceToCoast()) }
      distances foreach { d => d must beGreaterThanOrEqualTo(0.0) and (d must beLessThanOrEqualTo(1.0))} // d in [0,1]
      true must beTrue
    }
  }

  "The AssignElevation process" should {
    val shaper = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8), threshold = 30)

    val map = AssignElevation(entry)
    "annotate ocean points with a 0 elevation" in {
      val props = map.vertexProps.project(map.mesh.vertices) _
      val oceans = props(Set(WaterKind(ExistingWaterKind.OCEAN))) map { map.mesh.vertices(_).get }
      oceans foreach { map.vertexProps.check(_, HasForHeight(0)) must beTrue }
      true must beTrue // glitch to allow implicit conversion (thus compilation). real test is above.
    }
    "annotate coastline with a minimal elevation (0.1)" in {
      true must beTrue
    }
  }







}