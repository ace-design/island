package eu.ace_design.island.game

import eu.ace_design.island.map.resources.Resource

/**
 * A game is used to store the information about a given game
 * @param budget the budget available to play with
 * @param crew  the crew available to explore the island
 * @param objectives the objectives given by the
 * @param visited the locations visited by the explorer
 * @param boat The location of the boat
 */
class Game private(val budget: Budget,
                   val crew: Crew,
                   val objectives: Set[(Resource, Int)],
                   val visited: Set[(Int, Int)],
                   val boat: Option[(Int, Int)]) {

  /**
   * Update the current game based on the contents of the result of an action
   * @param res
   * @return a game
   */
  def updatedBy(res: Result): (Game, Result) = res.ok match {
    case false => (this, res)
    case true => {
      val remaining = budget - res.cost
      (new Game(remaining, crew, objectives, visited, boat), res)
    }
  }

  def moveBoat(loc: (Int,Int)): Game = new Game(budget, crew, objectives, visited, Some(loc))


}
object Game {
  def apply(budget: Budget, crew: Crew, objectives: Set[(Resource, Int)]) =
    new Game(budget,crew, objectives, visited = Set(), boat = None)
}


/**
 * A budget represent the complete amount of action points available, and the remaining ones.
 */
class Budget private(val initial: Int, val remaining: Int) {
  require(initial > 0, "Initial budget cannot be negative")
  // spending action points
  def -(cost: Int): Budget = {
    val r = this.remaining - cost
    if (r < 0)
      NotEnoughBudgetException(remaining, cost) // error case
    new Budget(initial, r)
  }
}
object Budget {
  def apply(value: Int) = new Budget(value, value)
}

class NotEnoughBudgetException(message: String) extends Exception(message)
object NotEnoughBudgetException {
  def apply(avail: Int, needed: Int) = {
    val m = s"Not enough budget to perform the requested action: needed $needed, available $avail"
    throw new NotEnoughBudgetException(m)
  }
}

/**
 * A crew represents the number of men available on the boat, the number of men used for the exploration and the
 * number of men currently on the island
 */
class Crew private(val complete: Int, val used: Int, val landed: Int) {

}
object Crew { def apply(men: Int) = new Crew(men, 0, 0) }