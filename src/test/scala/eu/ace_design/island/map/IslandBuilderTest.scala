package eu.ace_design.island.map

import org.specs2.mock.Mockito
import org.specs2.mutable._
import eu.ace_design.island.geom.{Mesh, MeshBuilder, SquaredGrid}

class IslandBuilderTest extends SpecificationWithJUnit with Mockito {

  "IslandBuilderTest Specifications".title

  "The IslandBuilder" should {

    val m = IslandMap(Mesh(size = Some(300)), PropertySet(), PropertySet())
    val p1 = mock[Process]; p1.apply(m) answers { im => im.asInstanceOf[IslandMap]}
    val p2 = mock[Process]; p2.apply(m) answers { im => im.asInstanceOf[IslandMap]}

    case class MockedBuilder(override val size: Int) extends IslandBuilder { val steps = Seq(p1, p2) }

    "reject meshes of unknown size" in {
      val builder = MockedBuilder(200)
      builder(Mesh()) must throwAn[IllegalArgumentException]
    }

    "reject meshes with size different to its own size" in {
      val builder = MockedBuilder(200)
      builder(Mesh(size = Some(300))) must throwAn[IllegalArgumentException]
    }

    "call each process stored in steps once" in {
      val builder = MockedBuilder(m.mesh.size.get)
      builder(m.mesh)
      there was one(p1).apply(m)
      there was one(p2).apply(m)
    }
  }
}

class ProcessTest extends SpecificationWithJUnit {
  final val SIZE = 600
  final val FACES = 100

  val generator = new SquaredGrid(SIZE) // A rectangular grid, 600 x 600.
  val builder = new MeshBuilder(SIZE)   // A mesh builder for 600 x 600 grids
  val mesh = builder(generator(FACES))  // generating the mesh, with 100 faces involved in the grid
  val entry = IslandMap(mesh)

  "The IdentifyBorders process" should {
    "annotate with IsBorder the faces that touch the external boundary" in {
      val updated = IdentifyBorders(entry)
      updated.vertexProps.size must_== 0
      updated.faceProps.size   must_== 4*(math.sqrt(FACES).toInt-1)
    }
  }

  "The IdentifyWaterArea process" should {
    val process = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble/2*0.8), threshold = 30)

    "annotate all the faces with IsWater properties" in {
      val updated = process(entry)
      val props = updated.faceProps.project(updated.mesh.faces) _
      val waters = props(Set(IsWater()))
      val lands  = props(Set(!IsWater()))
      waters ++ lands must_== mesh.faces.values
    }

    "consider border as water areas" in {
      val map = process(IdentifyBorders(entry))
      val props = map.faceProps.project(map.mesh.faces) _
      val borders = props(Set(IsBorder()))
      val waters =  props(Set(IsWater()))
      borders & waters must_== borders
    }
  }

  "The IdentifyLakesAndOcean process" should {
    "annotate all the water faces with WaterKind properties" in {
      import ExistingWaterKind.{OCEAN, LAKE}
      val donutsFactory = IdentifyWaterArea(shape = DonutShape(SIZE, SIZE.toDouble/2*0.8, SIZE.toDouble/2*0.2),
                                                               threshold = 30)
      val updated = IdentifyLakesAndOcean(donutsFactory(entry))
      val props = updated.faceProps.project(updated.mesh.faces) _
      val waters = props(Set(IsWater()))
      val oceans = props(Set(WaterKind(OCEAN)))
      val lakes =  props(Set(WaterKind(LAKE)))
      lakes ++ oceans must_== waters
    }
  }

  "The IdentifyCoastLine process" should {
    val shaper = IdentifyWaterArea(shape = DiskShape(SIZE, SIZE.toDouble/2*0.8), threshold = 30)

    "annotate land faces with an IsCoast tag" in {
      val map =  IdentifyCoastLine(shaper(entry))
      val finder = map.faceProps.project(map.mesh.faces) _
      val land = finder(Set(!IsWater()))
      val coast = finder(Set(IsCoast()))
      (coast & land) must_== coast
    }
  }
}