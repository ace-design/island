package eu.ace_design.island.viewer

import java.io.{PrintWriter, File}

import eu.ace_design.island.game.GameBoard
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.util.Logger
import org.json.{JSONArray, JSONObject}

case class PoiJSONViewer(board: GameBoard) extends Viewer with Logger {

  override def extension: String = "json"
  override def mimeType: String = "application/json"

  override def apply(m: IslandMap): File = {

    info("Building POI JSON File")
    val result = new JSONArray()
    board.pois.values.flatten foreach { poi =>
      poi.location match {
        case None =>
        case Some(p) => {
          val obj = new JSONObject()
          obj.put("kind",poi.name)
          obj.put("x", p.x)
          obj.put("y", p.y)
          obj.put("uid", poi.identifier)
          result.put(obj)
        }
      }
    }

    val out = initOutput
    val writer = new PrintWriter(out, "UTF8")
    result.write(writer)
    writer.close()
    out
  }

}
