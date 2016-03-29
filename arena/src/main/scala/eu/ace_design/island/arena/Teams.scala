package eu.ace_design.island.arena

import eu.ace_design.island.bot.IExplorerRaid
import eu.ace_design.island.arena.utils.Player

trait Teams {

  def players: Map[String, Class[_ <: IExplorerRaid]]

  def playerNames: String = players.map { case (n,_) => n.toUpperCase }.toSeq.sorted mkString ", "

  def asPlayers: Set[Player] = {
    (players map { case (name, bot) => Player(name = name, bot = bot) }).toSet
  }

}