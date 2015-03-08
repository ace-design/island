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
                   val boat: Option[(Int, Int)],
                   val isOK: Boolean = true) {

  /**
   * Update the current game based on the contents of the result of an action
   * @param res
   * @return a game
   */
  def updatedBy(res: Result): (Game, Result) = res.ok match {
    case false => (this, res)
    case true => {
      val remaining = budget - res.cost
      val g = res match {
        case e: EmptyResult => this.copy(budget = remaining)
        case m: MovedBoatResult => {
          this.copy(budget = remaining, crew = crew using m.men, boat = Some(m.loc), visited = visited + m.loc)
        }
      }
      (g, res)
    }
  }

  /**
   * Quickly tag a game as KO by changing its status
   * @return
   */
  def flaggedAsKO: Game = new Game(budget, crew, objectives, visited, boat, false)

  // copy a game into another one (simulating case class behavior)
  private def copy(budget: Budget = this.budget, crew: Crew = this.crew, objectives: Set[(Resource, Int)] = this.objectives,
           visited: Set[(Int, Int)] = this.visited, boat: Option[(Int, Int)] = this.boat, isOK: Boolean = this.isOK) =
    new Game(budget, crew, objectives, visited, boat, isOK)

}

object Game {
  def apply(budget: Budget, crew: Crew, objectives: Set[(Resource, Int)]) =
    new Game(budget,crew, objectives, visited = Set(), boat = None, true)
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
  require(complete > 1, "Not enough men in the crew")

  def using(m: Int) = new Crew(complete, used + m, m)
}
object Crew { def apply(men: Int) = new Crew(men, 0, 0) }