package eu.ace_design.island.stdlib

import eu.ace_design.island.map.resources.{ManufacturedResource, PrimaryResource, Resource}


/**
 * Represents the resources available in the game
 **/
object Resources  {

  val primaries: Set[PrimaryResource] = Set(FISH, QUARTZ, ORE, WOOD, FRUITS, SUGAR_CANE, FLOWER, FUR)

  object FISH extends PrimaryResource {
    override protected val perHectare = 40
    override protected val difficulty = 1.05
  }

  object QUARTZ extends PrimaryResource {
    override protected val perHectare = 5
    override protected val difficulty = 0.7
  }

  object ORE extends PrimaryResource {
    override protected val perHectare = 15
    override protected val difficulty = 0.5
  }

  object WOOD extends PrimaryResource {
    override protected val perHectare = 40
    override protected val difficulty = 1.05
  }

  object FRUITS extends PrimaryResource {
    override protected val perHectare = 10
    override protected val difficulty = 1.0
  }
  object SUGAR_CANE extends PrimaryResource {
    override protected val perHectare = 20
    override protected val difficulty = 0.8
  }

  object FLOWER extends PrimaryResource {
    override protected val perHectare = 1
    override protected val difficulty = 0.2
  }
  object FUR extends PrimaryResource {
    override protected val perHectare = 5
    override protected val difficulty = 0.95
  }

  val manufactured: Set[ManufacturedResource] = Set()
  //val GLASS, INGOT, PLANK, RUM, ELIXIR, LEATHER

  val values: Set[Resource] = primaries ++ manufactured

}


