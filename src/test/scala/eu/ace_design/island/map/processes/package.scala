package eu.ace_design.island.map

import eu.ace_design.island.geom.MeshBuilder
import eu.ace_design.island.geom.generators.SquaredGrid

package object processes {

  final val SIZE = 600
  final val FACES = 100

  val generator = new SquaredGrid(SIZE)

  // A rectangular grid, 600 x 600.
  val builder = new MeshBuilder(SIZE)

  // A mesh builder for 600 x 600 grids
  val mesh = builder(generator(FACES))

  // generating the mesh, with 100 faces involved in the grid
  val entry = IslandMap(mesh)

}
