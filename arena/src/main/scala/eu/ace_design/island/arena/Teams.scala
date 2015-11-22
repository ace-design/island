package eu.ace_design.island.arena

import eu.ace_design.island.bot.IExplorerRaid

trait Teams {

  def players: Map[String, IExplorerRaid]

  def playerNames: String = players.map { case (n,_) => n.toUpperCase }.toSeq.sorted mkString ", "

}