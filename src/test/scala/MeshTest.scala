import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MeshTest  extends SpecificationWithJUnit {
  "Island Map meshing specification".title

  "A mesh" should {
    val size = 200
    val chunk = 4
    "expose its faces" in {
      val  mesh = new SquaredMesh(size, chunk)
      mesh.faces must haveSize(chunk)
    }
  }

  " A squared mesh" should {
    val  mesh = new SquaredMesh(size = 200, chunk = 4)
    "reject a negative chunk size" in {
      new SquaredMesh(200, -1) must throwAn[IllegalArgumentException]
    }
    "reject a non squared chunk size" in {
      new SquaredMesh(200, 5) must throwAn[IllegalArgumentException]
    }
    "reject a negative or null plane size" in {
      new SquaredMesh(0, 4) must throwAn[IllegalArgumentException]
      new SquaredMesh(-200, 4) must throwAn[IllegalArgumentException]
    }
  }

}
