package eu.ace_design.island.arena.utils

import eu.ace_design.island.bot.IExplorerRaid
import eu.ace_design.island.game.{ExplorationEvent, Plane}
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.map.resources.Resource
import org.json.JSONObject
import org.json.JSONArray

import scala.language.existentials

case class Player(name: String, bot: Class[_ <: IExplorerRaid]) {

  override def toString: String = name

}

case class Job(islandData: IslandData, contract: Contract) {

  override def toString: String = {
    islandData.toString + ": " + contract.toString
  }

  def toJson: JSONObject = {
    new JSONObject()
      .put("island", islandData.name)
      .put("contract", contract.toJson)
  }

}

case class IslandData(island: IslandMap, seed: Long, name: String) {
  override def toString: String = name
}

case class Contract(crew: Int, budget: Int, plane: Plane, objectives: Set[(Resource, Int)])  {

  override def toString: String = {
     "Action points: " + budget + ", loc: " + plane.initial + ", crew: " +
       crew  + ", contracts: " + (objectives.mkString("[",",","]"))
  }

  def toJson: JSONObject = {
    val objs = new JSONArray()
    objectives foreach { o => objs.put(new JSONObject().put("res", o._1).put("amount",o._2)) }
    new JSONObject()
      .put("budget", budget)
      .put("crew", crew)
      .put("start", new JSONObject().put("x", plane.initial._1).put("y", plane.initial._2).put("h", plane.heading))
      .put("objs",new JSONArray().put(objs))
  }
}

trait Result {
  val name: String
  val islandName: String
  val execTime: Long
  val events: Seq[ExplorationEvent]

  def toJson: JSONObject
  def withExecTime(t: Long): Result

}

case class OK(override val name: String, override val islandName: String,
              remaining: Int, resources: Set[(Resource, Int)],
              override val events: Seq[ExplorationEvent],
              override val execTime: Long = 0) extends Result  {

  override def toJson: JSONObject = {
    val res = new JSONArray()
    resources foreach { r =>res.put(new JSONObject().put("res", r._1).put("amount",r._2)) }

    new JSONObject()
      .put("result", "OK")
      .put("player", name)
      .put("island", islandName)
      .put("remaining", remaining)
      .put("collected", res)
      .put("ms", execTime)
  }

  override def withExecTime(t: Long): Result = { this.copy(execTime = t) }

}

case class KO(override val name: String, override val islandName: String, reason: String,
              override val events: Seq[ExplorationEvent],
              override val execTime: Long = 0) extends Result {

  override def toJson: JSONObject = {
    new JSONObject()
      .put("result", "KO")
      .put("player", name)
      .put("island", islandName)
      .put("reason", reason)
      .put("ms",execTime)
  }

  override def withExecTime(t: Long): Result = { this.copy(execTime = t) }

}