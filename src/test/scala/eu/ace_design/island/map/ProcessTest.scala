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
    val updated = IdentifyBorders(entry)
    "annotate with IsBorder the faces that touch the external boundary" in {
      updated.faceProps.size must_== 4 * (math.sqrt(FACES).toInt - 1)
    }
    "annotate as border the points that touches the edge of the map" in {
      val borderPoints = updated.vertexProps.project(updated.mesh.vertices)(Set(IsBorder()))
      val check = (p: Point) => p.x == 0 || p.x == updated.mesh.size.get || p.y == 0 || p.y == updated.mesh.size.get
      borderPoints foreach { check(_) must beTrue }
      updated.vertexProps.size must beGreaterThan(0)
    }
  }

  "The IdentifyWaterArea process" should {
    val process = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8), threshold = 30)

    "annotate all the faces with IsWater properties" in {
      val updated = process(entry)
      val props = updated.faceProps.project(updated.mesh.faces) _
      val waters = props(Set(IsWater()))
      val lands = props(Set(!IsWater()))
      waters ++ lands must_== mesh.faces.values
    }

    "consider border as water areas" in {
      val map = process(IdentifyBorders(entry))
      val props = map.faceProps.project(map.mesh.faces) _
      val borders = props(Set(IsBorder()))
      val waters = props(Set(IsWater()))
      borders & waters must_== borders
    }
  }

  "The IdentifyLakesAndOcean process" should {
    "annotate all the water faces with WaterKind properties" in {
      import eu.ace_design.island.map.ExistingWaterKind.{LAKE, OCEAN}
      val donutsFactory = IdentifyWaterArea(shape = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2),
        threshold = 30)
      val updated = IdentifyLakesAndOcean(donutsFactory(entry))
      val props = updated.faceProps.project(updated.mesh.faces) _
      val waters = props(Set(IsWater()))
      val oceans = props(Set(WaterKind(OCEAN)))
      val lakes = props(Set(WaterKind(LAKE)))
      lakes ++ oceans must_== waters
    }
  }

  "The IdentifyCoastLine process" should {
    val shaper = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8), threshold = 30)

    "annotate land faces with an IsCoast tag" in {
      val map = IdentifyCoastLine(shaper(entry))
      val finder = map.faceProps.project(map.mesh.faces) _
      val land = finder(Set(!IsWater()))
      val coast = finder(Set(IsCoast()))
      (coast & land) must_== coast
    }
  }
}