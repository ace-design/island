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

  // return the extra information associated to this result.
  protected def extras(): JSONObject

  /**
   * Perform the object to json string transformation
   * @return
   */
  def toJson: String = {
    val result = new JSONObject()
    result.append("extras", extras())
    result.append("cost", cost)
    result.append("status", if (ok) "OK" else "KO")
    result.toString
  }
}

/**
 * Result returned with no extra information
 * @param cost
 */
case class EmptyResult(override val cost: Int) extends Result {
  override val ok: Boolean = true
  override protected def extras(): JSONObject = new JSONObject()
}

/**
 * Result returned when an exception occurs in the game system
 * @param e
 */
case class ExceptionResult(e: Exception) extends Result {
  override val ok: Boolean = false
  override val cost: Int = 0
  override protected def extras(): JSONObject = {
    val result = new JSONObject()
    result.append("exception", e.getClass.getName)
    result.append("message", e.getMessage)
    result.append("stacktrace", e.toString)
    result
  }
}
