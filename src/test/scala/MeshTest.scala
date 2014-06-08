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
    "create faces using a grid-based representation" in {
      val cs = Seq(Point(50, 50), Point(150, 50), Point(50, 150),
                   Point(150, 150))
      val ps = Seq(Point(0,0), Point(100, 0), Point(200, 0),
                   Point(0,100), Point(100, 100), Point(200, 100),
                   Point(0,200), Point(100, 200), Point(200, 200))
      val f1 = Face(cs(0), Seq(ps(0), ps(1), ps(4), ps(3)))
      val f2 = Face(cs(1), Seq(ps(1), ps(2), ps(5), ps(4)))
      val f3 = Face(cs(2), Seq(ps(3), ps(4), ps(7), ps(6)))
      val f4 = Face(cs(3), Seq(ps(4), ps(5), ps(8), ps(7)))
      mesh.faces must contain(f1,f2,f3,f4).exactly
    }
  }

}
