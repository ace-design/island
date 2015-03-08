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
}

/**
 * Result returned with no extra information
 * @param cost
 */
case class EmptyResult(override val cost: Int, override val shouldStop: Boolean = false) extends Result {
  override val ok: Boolean = true
  override protected def extras(): JSONObject = new JSONObject()
}

case class MovedBoatResult(override val cost: Int, loc: (Int,Int), val men: Int) extends Result {
  override val ok: Boolean = true
  override val shouldStop: Boolean = false
  override protected def extras(): JSONObject = new JSONObject()
}

case class MovedCrewResult(override val cost: Int, loc: (Int,Int)) extends Result {
  override val ok: Boolean = true
  override val shouldStop: Boolean = false
  override protected def extras(): JSONObject = new JSONObject()
}


case class ScoutResult(override val cost: Int,
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
}

case class ExploitResult(override val cost: Int, amount: Int, r: Resource) extends Result {
  override val ok: Boolean = true
  override val shouldStop: Boolean = false
  override protected def extras(): JSONObject = {
    val result = new JSONObject()
    result.put("amount", amount)
    result
  }
}

/**********************************
 * Exploit result  data structure *
 **********************************/
object ResourceLevels extends Enumeration {
  type ResourceLevel = Value ; val HIGH, MEDIUM, LOW = Value
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

case class ExploreResult(override val cost: Int,
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
}
