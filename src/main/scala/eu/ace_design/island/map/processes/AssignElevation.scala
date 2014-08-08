package eu.ace_design.island.map.processes

import eu.ace_design.island.geom.EdgeRegistry
import eu.ace_design.island.map.{PropertySet, HasForHeight, IsBorder, IslandMap}


object AssignElevation extends Process {

  override def apply(m: IslandMap): IslandMap = {
    info("Assigning initial elevation")
    val vFinder = m.vertexProps.project(m.mesh.vertices) _
    val borders = vFinder(Set(IsBorder())) map { m.mesh.vertices(_).get }
    val others = m.mesh.vertices.references diff borders ; val MAX = Double.PositiveInfinity
    val init = m.vertexProps bulkAdd(borders -> HasForHeight(0)) bulkAdd(others -> HasForHeight(MAX))

    info("Propagating elevation, starting from borders")
    val elevated = propagate(m, init, borders)


    m.copy(vertexProps = elevated)

  }


  private def propagate(m: IslandMap, props: PropertySet, points: Set[Int]): PropertySet = points.headOption match {
    case None => props
    case Some(pRef) => {
      val pElevation = props.getValue(pRef, HasForHeight())
      val nElevation = pElevation + 0.01
      m.mesh.edges.adjacents(pRef)

    }
  }


}
