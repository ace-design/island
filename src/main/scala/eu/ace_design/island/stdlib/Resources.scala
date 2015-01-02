package eu.ace_design.island.stdlib

import eu.ace_design.island.map.resources.{ManufacturedResource, PrimaryResource, Resource}


/**
 * Represents the resources available in the game
 **/
object Resources  {

  val primaries: Set[PrimaryResource] = Set(FISH, SILICA, ORE, WOOD, FRUITS, SUGAR_CANE, FLOWER, FUR)

  object FISH extends PrimaryResource {

  }

  object SILICA extends PrimaryResource {

  }

  object ORE extends PrimaryResource {

  }

  object WOOD extends PrimaryResource {

  }
  object FRUITS extends PrimaryResource {

  }
  object SUGAR_CANE extends PrimaryResource {

  }
  object FLOWER extends PrimaryResource {

  }
  object FUR extends PrimaryResource {

  }

  val manufactured: Set[ManufacturedResource] = Set()
  //val GLASS, INGOT, PLANK, RUM, ELIXIR, LEATHER

  val values: Set[Resource] = primaries ++ manufactured

}


