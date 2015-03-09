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
                   val isOK: Boolean = true,
                   val extracted: Map[Resource,Map[(Int,Int),Int]] = Map()) {

  /**
   * Update the current game based on the contents of the result of an action
   * @param res
   * @return a game
   */
  def updatedBy(res: Result): (Game, Result) = res.ok match {
    case false => (this, res)
    case true => {
      val g = res match {
        case e: EmptyResult   => this // no side effect (except on budget)
        case s: ScoutResult   => this // no side effect (except on budget)
        case e: ExploreResult => this // no side effect (except on budget)
        case m: MovedBoatResult => {
          val updatedCrew = crew movedTo m.loc using m.men
          this.copy(crew = updatedCrew, boat = Some(m.loc), visited = visited + m.loc)
        }
        case  m: MovedCrewResult => {
          val updatedCrew = crew movedTo m.loc
          this.copy(crew = updatedCrew, visited = visited + m.loc)
        }
        case e: ExploitResult => harvest(e.r, crew.location.get, e.amount)
        case _ => throw new UnsupportedOperationException("Game cannot handle update with " + res)
      }
      val remaining = budget - ( Game.MINIMAL_COST_FOR_ACTION + res.cost )
      (g.copy(budget = remaining), res)
    }
  }

  def harvested(r: Resource, loc: (Int, Int)): Int = {
    val forResource = extracted.getOrElse(r,Map())
    forResource.getOrElse(loc,0)
  }

  def harvest(r: Resource, loc: (Int, Int), amount: Int): Game = {
    val updated = extracted.get(r) match {
      case None => extracted + (r -> Map(loc -> amount))
      case Some(data) => data.get(loc) match {
        case None => extracted + (r -> (data + (loc -> amount)))
        case Some(old) => extracted + (r -> (data + (loc -> (old + amount))))
      }
    }
    this.copy(extracted = updated)
  }

  /**
   * Quickly tag a game as KO by changing its status
   * @return
   */
  def flaggedAsKO: Game = new Game(budget, crew, objectives, visited, boat, false)

  // copy a game into another one (simulating case class behavior)
  private def copy(budget: Budget = this.budget, crew: Crew = this.crew,
                   objectives: Set[(Resource, Int)] = this.objectives,
                   visited: Set[(Int, Int)] = this.visited, boat: Option[(Int, Int)] = this.boat,
                   isOK: Boolean = this.isOK, extracted: Map[Resource,Map[(Int,Int),Int]] = this.extracted) =
    new Game(budget, crew, objectives, visited, boat, isOK, extracted)

}

object Game {
  final val MINIMAL_COST_FOR_ACTION = 2   // TODO: refactor [An action costs at min 2 action points]

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
class Crew private(val complete: Int, val used: Int, val landed: Int, val location: Option[(Int, Int)]) {
  require(complete > 1, "Not enough men in the crew")

  def using(m: Int) = this.copy(used = used + m, landed = m)

  def movedTo(loc: (Int, Int)) = this.copy(location = Some(loc))

  private def copy(complete: Int = this.complete,  used: Int = this.used,  landed: Int = this.landed,
                   location: Option[(Int, Int)] = this.location) = new Crew(complete, used, landed, location)


}
object Crew { def apply(men: Int) = new Crew(men, 0, 0, None) }