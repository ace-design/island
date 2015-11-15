package eu.ace_design.island.game

/**
  * This file is part of the island project
  * @author mosser (09/11/2015, 14:14)
  **/
package object actions {


  def exec(actions: Seq[Result], g: Game): Game = {
    if (actions.isEmpty) g else exec(actions.tail, (g updatedBy actions.head)._1)
  }

}
