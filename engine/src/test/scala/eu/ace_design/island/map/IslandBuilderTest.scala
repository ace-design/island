package eu.ace_design.island.map

import org.specs2.mock.Mockito
import org.specs2.mutable._
import eu.ace_design.island.geom.Mesh

class IslandBuilderTest extends SpecificationWithJUnit with Mockito {

  "IslandBuilderTest Specifications".title

  "The IslandBuilder" should {

    val mesh = Mesh(size = Some(300))
    val m = IslandMap(mesh)
    val p1 = mock[processes.Process];
    //p1.apply(m) answers { im: IslandMap => im.asInstanceOf[IslandMap]}
    val p2 = mock[processes.Process];
    //p2.apply(m) answers { im: IslandMap => im.asInstanceOf[IslandMap]}

    case class MockedBuilder(override val size: Int) extends IslandBuilder { val steps = Seq(p1, p2) }

    "reject meshes of unknown size" in {
      val builder = MockedBuilder(200)
      builder(Mesh()) must throwAn[IllegalArgumentException]
    }

    "reject meshes with size different to its own size" in {
      val builder = MockedBuilder(200)
      builder(Mesh(size = Some(300))) must throwAn[IllegalArgumentException]
    }

    /*
    "call each process stored in steps once" in {
      val builder = MockedBuilder(m.size)
      builder(mesh)
      there was one(p1).apply(m)
      there was one(p2).apply(m)
    }

     */
  }

}