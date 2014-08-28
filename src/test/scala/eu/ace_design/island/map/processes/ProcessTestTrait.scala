package eu.ace_design.island.map.processes

import java.io.File

import eu.ace_design.island.geom.MeshBuilder
import eu.ace_design.island.geom.generators.SquaredGrid
import eu.ace_design.island.map.IslandMap
import org.specs2.mutable.SpecificationWithJUnit


trait ProcessTestTrait extends SpecificationWithJUnit {

  final val SIZE = 800
  final val FACES = 400

  protected val generator = new SquaredGrid(SIZE)

  // A rectangular grid, 600 x 600.
  protected val builder = new MeshBuilder(SIZE)

  // A mesh builder for 600 x 600 grids
  protected val mesh = builder(generator(FACES))

  // generating the mesh, with 100 faces involved in the grid
  protected val entry = IslandMap(mesh)


  protected val preconditions: IslandMap => IslandMap
  protected val updated: IslandMap


  protected def draw() {
    val name = s"./Test-${this.getClass.getSimpleName}.pdf"
    val pdf = new eu.ace_design.island.viewer.PDFViewer()
    pdf(updated).renameTo(new File(name))
  }


}
