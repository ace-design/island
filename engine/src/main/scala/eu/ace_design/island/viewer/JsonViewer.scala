package eu.ace_design.island.viewer

import java.io.{PrintWriter, File}
import eu.ace_design.island.geom._
import eu.ace_design.island.map.processes.Statistics
import org.json
import org.json._
import eu.ace_design.island.map._
import eu.ace_design.island.util.Logger

/**
 * Transform a map into a JSON file. The JSON file is directly inspired by the OBJ WaveFront file format for
 * vertices and faces indexes.
 **/
object JsonViewer extends Viewer with Logger {

  override def extension: String = "json"
  override def mimeType: String = "application/json"

  override def apply(m: IslandMap): File = {

    info("Building JSON file")
    val vertices = buildVertices(m)
    val faces = buildFaces(m)
    val vProps = buildProperties(m.vertexProps.keep(Set()))
    val fProps = buildProperties(m.faceProps.keep(Set(HasForBiome(), HasForCondition(), HasForSoil())))


    val geom = new JSONObject()
    geom.put("vertices", vertices)
    geom.put("faces", faces)


    val props = new JSONObject()
    props.put("vertices", vProps)
    props.put("faces", fProps)

    val result = new JSONObject()
    result.put("geometry", geom)
    result.put("properties", props)
    m.stats match {
      case None =>
      case Some(stats) => result.put("stats", buildStatistics(stats))
    }
    result.put("_comments", "JSON representation of an Island, automatically generated")

    val out = initOutput
    val writer = new PrintWriter(out, "UTF8")
    result.write(writer)
    writer.close()
    out
  }


  private def buildVertices(m: IslandMap): JSONArray = {
    info("Building vertices index")
    val data = ViewerHelpers.buildVertices(m) map { c => new JSONArray().put(c._1).put(c._2).put(c._3) }
    (new JSONArray() /: data) { (acc, vertex) => acc.put(vertex) }
  }

  private def buildFaces(m: IslandMap): JSONArray = {
    info("Building faces index")
    val data = ViewerHelpers.buildFaces(m) map { f => (new JSONArray() /: f) { (acc, ref) => acc.put(ref) }}
    (new JSONArray() /: data) { (acc, face) => acc.put(face)}
  }

  /**
   * Build a property set representation. { "i": reference, "p": { "prop1": val1, ... } }
   * i is the referenced index, and p contains the stored properties as an object (key-value pairs)
   * @param reg
   * @return
   */
  def buildProperties(reg: PropertySet): JSONArray = {
    val objects = reg.references.toSeq.sorted map { ref =>
      (new JSONObject() /: reg.get(ref)) { (acc, prop) => acc.put(prop.key,prop.value) }
    }
    (new JSONArray() /: objects) { (acc, obj) => acc.put(obj) }
  }

  /**
   * Build a JSON array of statistics stored in a map
   * @param stats
   * @return
   */
  def buildStatistics(stats: Map[Statistics.StatName, String]): JSONArray = {
    val objects = stats map { case (k, v) =>
      new JSONObject().put("id", k.id).put("stat", k.toString).put("value", v)
    }
    (new JSONArray() /: objects) { (acc, obj) => acc.put(obj) }
  }


}
