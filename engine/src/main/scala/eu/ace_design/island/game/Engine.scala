package eu.ace_design.island.game

import eu.ace_design.island.bot.IExplorerRaid

/**
 * The engine is used to play a party
 **/
class Engine(val board: GameBoard, val game: Game) {

  // run a game using a given explorer. Use mutable state (=> UGLY)
  def run(explorer: IExplorerRaid): Seq[ExplorationEvent] = {

    // mutable elements used to model the game
    var endOfGame = false
    var error: Boolean = false
    val events = scala.collection.mutable.Seq[ExplorationEvent]()

    try { explorer.initialize("") } catch { case e: Exception => }
    while (!endOfGame) {
      // retrieving users decision
      val decision: String = try { explorer.takeDecision() } catch { case e: Exception => error = true; e.getMessage }
      events :+ ExplorationEvent(Actors.Explorer, decision)
      //if (error) break;
      // executing action

      try { explorer.acknowledgeResults("") } catch { case e: Exception => }
      if (false) {endOfGame = true }
    }
    events
  }

}


object Actors extends Enumeration {
  type Actor = Value
  val Explorer, Engine = Value
}

object ExplorationEvent {
  def apply(actor: Actors.Actor, data: String) = new ExplorationEvent(System.currentTimeMillis(), actor, data)
}
class ExplorationEvent private(val timestamp: Long, val actor: Actors.Actor, data: String) {

}