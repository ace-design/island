package eu.ace_design.island.map

import org.specs2.mutable._
import eu.ace_design.island.geom.{MeshBuilder, SquaredGrid}

class IslandBuilderTest extends SpecificationWithJUnit {

  "IslandBuilderTest Specifications".title

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