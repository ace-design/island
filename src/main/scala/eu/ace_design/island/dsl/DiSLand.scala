package eu.ace_design.island.dsl

import eu.ace_design.island.geom.PointGenerator
import eu.ace_design.island.map._

/**
 * This file is part of the island project
 * @author mosser (05/08/2014, 14:43)
 **/
trait DiSLand {




  private case class Config (size: Option[Int], generator: Option[PointGenerator], shape: Option[IslandShape],
                     waterThreshold: Option[Int], process: Seq[Process])



}
