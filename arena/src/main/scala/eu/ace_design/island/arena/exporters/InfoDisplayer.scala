package eu.ace_design.island.arena.exporters

import eu.ace_design.island.game.{Game, GameBoard, PointOfInterest}
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.stdlib.PointOfInterests.{Creek, EmergencySite}


trait InfoDisplayer {

  final def apply(isl: IslandMap, board: GameBoard, game: Game) {
    println(s"\n## $title")
    process(isl, board, game)
    println()
  }

  protected val title: String
  protected def process(isl: IslandMap, board: GameBoard, game: Game)

}

object IslandStatistics extends InfoDisplayer {
  override protected val title = "Global statistics"

  override def process(isl: IslandMap, board: GameBoard, game: Game) {
    isl.stats match {
      case None =>
      case Some(d) => d.toSeq sortBy { _._1.toString  } foreach { case (stat, value) => println(s"  - $stat => $value") }
    }
  }
}


object ResourcesInfo extends InfoDisplayer {
  override protected val title: String = "Resources amounts"

  override protected def process(isl: IslandMap, board: GameBoard, game: Game) = {
    board.contents.toSeq.sortBy(_._2).reverse foreach { case (res, amount) =>
      println(f"  - ${res}%-10s => $amount")
    }
  }
}


object POIInfo extends InfoDisplayer {
  override protected val title = "Point of Interests available"

  override protected def process(isl: IslandMap, board: GameBoard, game: Game) {
    board.pois foreach { case (loc, pois) => {
      val data = pois map { p =>
        p.name +
          (if(p.location.isDefined) {f"@(${p.location.get.x}%.2f,${p.location.get.y}%.2f)"} else { "" }) +
          s" [${p.identifier}]"
      }
      println(s"  - $loc: ${ data mkString("(",",",")") }")
    }}
  }
}

object EmergencyDistance extends InfoDisplayer {
  override protected val title = "Emergency site & Creeks"

  override protected def process(isl: IslandMap, board: GameBoard, game: Game) {
    val creeks = board.findPOIsByType(Creek("aCreek",None)).map{ _._2 }.toList
    val site = board.findPOIsByType(EmergencySite("aSite",None)).map{ _._2 }.head

    println(s"  - Emergency site [${site.name}] located at (${site.location.get.x},${site.location.get.y})\n")

    creeks.sortWith { (close, far) => dist(close, site) < dist(far, site) } foreach { creek =>
      println(s"  1. Creek [${creek.name}] located at (${creek.location.get.x},${creek.location.get.y}), distance: ${dist(creek,site)}")
    }
  }

  private def dist(a: PointOfInterest, b: PointOfInterest): Double = a.location.get --> b.location.get
}

object MapInfo extends InfoDisplayer {
  override protected val title = "Island Map information"

  override protected def process(isl: IslandMap, board: GameBoard, game: Game): Unit = {
    println(s"  - Map Size:  ${isl.size}x${isl.size}")
    println(s"  - Tile Size: ${board.tileUnit}x${board.tileUnit}")
    if(board.startingTile.isDefined) {
      println(s"  - Starting tile: ${board.startingTile.get}")
    }
    println(s"  - Underlying Mesh: ")
    println(s"    - |Vertices|: ${isl.vertices.size}")
    println(s"    - |Edges|:    ${isl.edges.size}")
    println(s"    - |Faces|:    ${isl.faces.size}")
  }
}

object ObjectiveInfo extends InfoDisplayer {
  override protected val title = "Objectives"

  override def process(isl: IslandMap, board: GameBoard, game: Game) {
    game.objectives foreach { case (res, amount) => println(f"  - ${res.name}%-10s: $amount") }
  }
}