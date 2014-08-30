package eu.ace_design.island.map.processes

import java.io.File

import eu.ace_design.island.geom.MeshBuilder
import eu.ace_design.island.geom.generators.SquaredGrid
import eu.ace_design.island.map.IslandMap
import org.specs2.mutable.SpecificationWithJUnit


trait ProcessTestTrait extends SpecificationWithJUnit {

  /**
   * The preconditions to apply to a map to make it an acceptable entry for the process under test
   */
  protected val preconditions: IslandMap => IslandMap

  /**
   * The process under test, to be applied to 'entry' after 'preconditions' to produce 'result'
   */
  protected val processUnderTest: Process

  final val SIZE = 800
  final val FACES = 400

  private val generator = new SquaredGrid(SIZE)

  // A rectangular grid, 600 x 600.
  private val builder = new MeshBuilder(SIZE)

  // A mesh builder for 600 x 600 grids
  private val mesh = builder(generator(FACES))

  // generating the mesh, with 100 faces involved in the grid
  protected final val entry = IslandMap(mesh)
  protected final val result: IslandMap = processUnderTest(preconditions(entry))

  /**
   * Draw the result map into a PDF file (usually for debug purpose)  
   */
  protected def draw() {
    val name = s"./Test-${this.getClass.getSimpleName}.pdf"
    val pdf = new eu.ace_design.island.viewer.PDFViewer()
    pdf(result).renameTo(new File(name))
  }


}
