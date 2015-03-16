package eu.ace_design.island.game

import eu.ace_design.island.map.resources.Resource
import eu.ace_design.island.map.resources.Conditions
import org.json.{JSONArray, JSONObject}

/**
 * Results of actions
 * Results are defined as { "status": {OK, KO}, "cost": n, "extras": { ... }}
 */
trait Result {

  // Did the action performed well?
  val ok: Boolean
  //What was the cost of this action
  val cost: Int
  // should the game be stopped
  val shouldStop: Boolean


  // return the extra information associated to this result.
  protected def extras(): JSONObject

  /**
   * Perform the object to json string transformation
   * @return
   */
  def toJson: JSONObject = {
    val result = new JSONObject()
    result.put("extras", extras())
    result.put("cost", cost)
    result.put("status", if (ok) "OK" else "KO")
  }

  def withCost(c: Int): Result

}

/**
 * Result returned with no extra information
 * @param cost
 */
case class EmptyResult(override val cost: Int = 0, override val shouldStop: Boolean = false) extends Result {
  override val ok: Boolean = true
  override protected def extras(): JSONObject = new JSONObject()
  def withCost(c: Int) = this.copy(cost = c)
}

case class MovedBoatResult(override val cost: Int = 0, loc: (Int,Int), men: Int) extends Result {
  override val ok: Boolean = true
  override val shouldStop: Boolean = false
  override protected def extras(): JSONObject = new JSONObject()
  def withCost(c: Int) = this.copy(cost = c)
}

case class MovedCrewResult(override val cost: Int= 0, loc: (Int,Int)) extends Result {
  override val ok: Boolean = true
  override val shouldStop: Boolean = false
  override protected def extras(): JSONObject = new JSONObject()
  def withCost(c: Int) = this.copy(cost = c)
}


case class ScoutResult(override val cost: Int= 0,
                       resources: Set[Resource], altitude: Int, unreachable: Boolean = false) extends Result {
  override val ok: Boolean = true
  override val shouldStop: Boolean = false
  override protected def extras(): JSONObject = {
    val result = new JSONObject()
    if (unreachable)
      result.put("unreachable", unreachable)

    result.put("altitude", altitude)
    result.put("resources", (resources map { _.name }).toArray)
    result
  }
  def withCost(c: Int) = this.copy(cost = c)
}

case class ExploitResult(override val cost: Int = 0, amount: Int, r: Resource) extends Result {
  override val ok: Boolean = true
  override val shouldStop: Boolean = false
  override protected def extras(): JSONObject = {
    val result = new JSONObject()
    result.put("amount", amount)
    result
  }
  def withCost(c: Int) = this.copy(cost = c)
}

/**********************************
 * Exploit result  data structure *
 **********************************/

case class ExploreResult(override val cost: Int = 0,
                         resources: Set[ResourceExploration], pois: Set[PointOfInterest])  extends  Result{
  override val ok: Boolean = true
  override val shouldStop: Boolean = false

  override protected def extras(): JSONObject = {
    val result = new JSONObject()
    result.put("resources", (resources map { _.toJson }).toArray)
    val jsonPOIS = new JSONArray()
    pois foreach { p =>
      val obj = new JSONObject()
      obj.put("kind", p.name)
      obj.put("id", p.identifier)
      jsonPOIS.put(obj)
    }
    result.put("pois", jsonPOIS)
    result
  }
  def withCost(c: Int) = this.copy(cost = c)

}

case class ResourceExploration(resource: Resource, amount: ResourceLevels.ResourceLevel,
                               condition: Conditions.Condition) {
  def toJson: JSONObject = {
    val result = new JSONObject()
    result.put("resource", resource.name)
    result.put("amount", amount)
    result.put("cond", condition)
    result
  }
}


case class ExceptionResult(e: Exception) extends Result {
  override val ok: Boolean = false
  override val cost: Int = 0
  override val shouldStop: Boolean = true

  override protected def extras(): JSONObject = {
    val result = new JSONObject()
    result.put("exception", e.getClass.getName)
    result.put("message", e.getMessage)
    result.put("stacktrace", e.toString)
    result
  }
  def withCost(c: Int) = this // cost not used for Exception result
}
