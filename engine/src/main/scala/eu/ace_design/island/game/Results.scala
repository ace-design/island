package eu.ace_design.island.game

import org.json.JSONObject

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


case class ExceptionResult(e: Exception) extends Result {
  override val ok: Boolean = false
  override val cost: Int = 0
  override val shouldStop: Boolean = true

  override protected def extras(): JSONObject = {
    val result = new JSONObject()
    result.append("exception", e.getClass.getName)
    result.append("message", e.getMessage)
    result.append("stacktrace", e.toString)
    result
  }
}
