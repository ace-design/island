package eu.ace_design.island.dsl

import eu.ace_design.island.geom._
import eu.ace_design.island.map._
import eu.ace_design.island.map.processes._
import eu.ace_design.island.viewer._

/**
 * This file is part of the island project
 * @author mosser (05/08/2014, 14:43)
 **/
trait DiSLand {

  // TODO fix issue in size propagation
  private object defaults {
    final val SIZE: Int  = 800
    final val FACES: Int = 600
    final val GENERATOR: PointGenerator = new RelaxedRandomGrid(SIZE)
    final val SHAPE: IslandShape        =  RadialShape(SIZE, 1.87)
    final val WATER_THRESHOLD: Int      = 30
    final val PROCESS = Seq(IdentifyBorders, IdentifyWaterArea(SHAPE, WATER_THRESHOLD),
                            IdentifyLakesAndOcean, IdentifyCoastLine)
    final val OUTPUT_FILE = "./map"
    final val VIEWER      = new PDFViewer()
  }

  def createIsland = Config()


  protected case class Config (size: Int               = defaults.SIZE,
                             faces: Int                = defaults.FACES,
                             generator: PointGenerator = defaults.GENERATOR,
                             shape: IslandShape        = defaults.SHAPE,
                             waterThreshold: Int       = defaults.WATER_THRESHOLD,
                             process: Seq[Process]     = defaults.PROCESS,
                             output: String            = defaults.OUTPUT_FILE,
                             format: Viewer            = defaults.VIEWER)
  {
    //require(generator.size == size, "Size specification must match in all parameters")
    //require(shape.size == size,     "Size specification must match in all parameters")

    def withSize(s: Int) = this.copy(size = s)
    def having(fi: FaceInteger) = this.copy(faces = fi.i)
    def shapedAs(s: IslandShape) = this.copy(shape = s)
    def storedIn(f: String) = this.copy(output = f)
    def as(v: Viewer) = this.copy(format = v)


    def toMap: IslandMap = {
      val sites = generator(faces)
      val meshBuilder = new MeshBuilder(size)
      val mesh = meshBuilder(sites)
      val s = size
      val mapBuilder = new IslandBuilder {
        override def size: Int = s
        override protected val steps: Seq[Process] = process
      }
      val map = mapBuilder(mesh)
      val out = format(map)
      out.renameTo(new java.io.File(output+"."+format.extension))
      map
    }

  }


  implicit protected def integerToFaceInteger(i: Int) = new FaceInteger(i)
  protected class FaceInteger(val i: Int) { def faces = i }

  protected val pdf = new PDFViewer()
  protected val svg = new SVGViewer()
  protected val obj = new OBJViewer()


  implicit protected def config2map(c: Config): IslandMap = c.toMap



}
