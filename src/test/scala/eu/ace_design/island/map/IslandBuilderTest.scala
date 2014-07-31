package eu.ace_design.island.map

import org.specs2.mock.Mockito
import org.specs2.mutable._
import eu.ace_design.island.geom.{Mesh, MeshBuilder, SquaredGrid}

class IslandBuilderTest extends SpecificationWithJUnit with Mockito {

  "IslandBuilderTest Specifications".title

   "The IslandBuilder" should {

     val m = IslandMap(Mesh(size = Some(300)), PropertySet(), PropertySet())
     val p1 = mock[Process]; p1.apply(m) answers { im => im.asInstanceOf[IslandMap] }
     val p2 = mock[Process]; p2.apply(m) answers { im => im.asInstanceOf[IslandMap] }

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

  "The IdentifyBorders process should" in {
    val generator = new SquaredGrid(200) // A rectangular grid, 200 x 200.
    val builder = new MeshBuilder(200)   // A mesh builder for 200 x 200 grids
    val mesh = builder(generator(16))    // generating the mesh, with 16 faces involved in the grid
    val entry = IslandMap(mesh)
    "annotate with IsBorder faces touching the external boundary" in {
      val updated = IdentifyBorders(entry)
      updated.vertexProps.size must_== 0
      updated.faceProps.size   must_== 12
    }
  }


}