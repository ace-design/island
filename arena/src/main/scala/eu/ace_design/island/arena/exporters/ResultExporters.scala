package eu.ace_design.island.arena.exporters

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}

import eu.ace_design.island.game.{ExplorationEvent, Game, GameBoard}
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.viewer.PoiJSONViewer
import eu.ace_design.island.viewer.svg.{FogOfWar, FogOfWarViewer}


trait ResultExporter { val outputDir: String }

trait CommonExporter extends ResultExporter {
  def apply(board: GameBoard, m: IslandMap)
}

trait PlayerExporter extends ResultExporter {}

case class POIsExporter(override val outputDir: String) extends CommonExporter {

  override def apply(board: GameBoard, m: IslandMap) {
    val viewer = PoiJSONViewer(board)
    val tmp = Paths.get(viewer(m).getAbsolutePath)
    val out = (new File(s"$outputDir/_pois.json")).toPath
    Files.move(tmp,out, StandardCopyOption.REPLACE_EXISTING)
  }

}

case class GameLogExporter(override val outputDir: String)  extends PlayerExporter {

  def apply(name: String, events: Seq[ExplorationEvent]): Unit = {
    val jsonEvents = events map { _.toJson } mkString("[", ",", "]")
    val writer = new PrintWriter(new File(s"$outputDir/$name.json"))
    try { writer.write(jsonEvents) } finally { writer.close() }
  }

}

case class VisitedMapExporter(override val outputDir: String) extends PlayerExporter {

  def apply(name: String, m: IslandMap, game: Game, tileUnit: Int, board: GameBoard) {
    val pois = board.pois.values.flatten map { _.location } filter { _.isDefined } map { _.get } map { p => (p.x,p.y) }
    val fog = new FogOfWar(factor = tileUnit, visited = game.visited, scanned = game.scanned, pois = pois.toSet, size = m.size)
    val viewer = FogOfWarViewer(fog)
    val tmp = Paths.get(viewer(m).getAbsolutePath)
    val out = (new File(s"$outputDir/$name.svg")).toPath
    Files.move(tmp,out, StandardCopyOption.REPLACE_EXISTING)
  }

}


