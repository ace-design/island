package eu.ace_design.island.game.actions

import eu.ace_design.island.game._
import org.json.JSONArray

/**
 * This file is part of the Default (Template) Project project
 * @author mosser (26/10/2015, 17:35)
 **/
case class Glimpse(range: Int, override val direction: Directions.Direction) extends ActionWithDirection {

  require(range > 0,  "Range for glimpse must be greater than zero")
  require(range <= 4, "Range for glimpse cannot exceed 4")

  override def computeCost(board: GameBoard, game: Game): Double = math.sqrt(range)

  override def buildResult(board: GameBoard, game: Game): Result = {
    require(game.crew.location.isDefined, "Cannot glimpse without having landed before")
    val start = game.crew.location.get
    val raw = ((1 to range) :\ Seq[JSONArray]()) { (idx, acc) => buildReport(moveTo(idx, start), idx, board) +: acc }
    val report = raw filterNot { _.length() == 0 }
    GlimpseResult(report = report, asked = range)
  }

  private def buildReport(position: (Int, Int), range: Int, board: GameBoard): JSONArray = {
    if (!board.tiles.contains(position))
      return new JSONArray()
    val tile = board.at(position._1, position._2)
    val contents = tile.biomes map { case (b,v) => new JSONArray().put(0,b.name).put(1,round(v)) }
    range match {
      case x if x <= 2 => (contents.toSeq.sortBy { _.getDouble(1) } :\ new JSONArray()) { (rep, acc) => acc.put(rep) }
      case 3 => (contents.map { _.getString(0) } :\ new JSONArray()) { (elem, acc) => acc.put(elem) }
      case 4 => new JSONArray().put(contents.toSeq.sortBy( _ .getDouble(1) ).last.getString(0))
    }
  }

  private def moveTo(idx: Int, acc: (Int, Int)): (Int, Int) = idx match {
    case 1 => acc
    case _ => moveTo(idx-1, Directions.move(acc._1, acc._2, direction))
  }

  private def round(v: Double) = (v * 100 * 100).round / 100.0


}
