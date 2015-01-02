package eu.ace_design.island.stdlib


/**
 * Represents the resources available in the game
 **/
object Resources extends Enumeration {
  type Resource = Value

  val NoResource = Value // The none resource

  // Primary Resources
  val FISH, SILICA, ORE, WOOD, FRUITS, SUGAR_CANE, FLOWER, FUR = Value

  // Transformed Resources
  val GLASS, INGOT, PLANK, RUM, ELIXIR, LEATHER = Value
}


