package eu.ace_design.island.io

import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.map.processes.Statistics
import org.json.JSONObject

/**
 * This file is part of the island project
 * @author mosser (05/08/2015, 14:10)
 **/
object IslandMapFactory {


  def apply(map: IslandMap): JSONObject = {
    val result = new JSONObject()
    result.put("mesh",         MeshFactory(map.mesh))
    result.put("vertex_props", PropertySetFactory(map.vertexProps))
    result.put("edge_props",   PropertySetFactory(map.edgeProps))
    result.put("face_props",   PropertySetFactory(map.faceProps))
    map.uuid match {
      case None =>
      case Some(id) => result.put("uuid", id)
    }
    map.stats match {
      case None =>
      case Some(stats) => result.put("stats", StatisticsFactory(stats))
    }
    result
  }

  def apply(obj: JSONObject): IslandMap = {
    val mesh = MeshFactory(obj.getJSONObject("mesh"))
    val uuid = obj.has("uuid") match {
      case true  => Some(obj.getLong("uuid"))
      case false => None
    }
    val stats = obj.has("stats") match {
      case true  => Some(StatisticsFactory(obj.getJSONObject("stats")))
      case false => None
    }
    val vProps = PropertySetFactory(obj.getJSONArray("vertex_props"))
    val eProps = PropertySetFactory(obj.getJSONArray("edge_props"))
    val fProps = PropertySetFactory(obj.getJSONArray("face_props"))
    IslandMap(mesh).copy(vertexProps = vProps, edgeProps = eProps, faceProps = fProps, uuid = uuid, stats = stats)
  }


  /**
   * Transform a map of statistics into a JSON object (bidirectional)
   */
  private object StatisticsFactory {

    // From Map to JSON
    def apply(data: Map[Statistics.StatName, String]): JSONObject = {
      val tmp = data map { case (k,v) => k.toString -> v }
      (new JSONObject() /: tmp) { case (json, (key,value)) => json.put(key,value)  }
    }

    // From JSON to map
    def apply(json: JSONObject): Map[Statistics.StatName, String] = {
      import collection.JavaConversions._
      (for(k <- json.keySet()) yield Statistics.withName(k) -> json.getString(k)).toMap
    }
  }
}
