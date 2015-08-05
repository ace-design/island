package eu.ace_design.island.io

import eu.ace_design.island.map.IslandMap
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
    result
  }

  def apply(obj: JSONObject): IslandMap = {
    val mesh = MeshFactory(obj.getJSONObject("mesh"))
    val uuid = obj.has("uuid") match {
      case true  => Some(obj.getLong("uuid"))
      case false => None
    }
    val vProps = PropertySetFactory(obj.getJSONArray("vertex_props"))
    val eProps = PropertySetFactory(obj.getJSONArray("edge_props"))
    val fProps = PropertySetFactory(obj.getJSONArray("face_props"))
    IslandMap(mesh).copy(vertexProps = vProps, edgeProps = eProps, faceProps = fProps, uuid = uuid)
  }

}
