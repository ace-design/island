package eu.ace_design.island.arena

import java.io._

import eu.ace_design.island.bot.IExplorerRaid
import eu.ace_design.island.game.{Engine, ExplorationEvent, Game, GameBoard}
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.map.resources.Resource
import eu.ace_design.island.viewer.svg.{FogOfWar, FogOfWarViewer}
import eu.ace_design.island.viewer.PoiJSONViewer

import scala.util.Random


trait Championship extends App with Teams {

  def outputDir: String
  val seed: Long
  val objectives: Set[(Resource, Int)]

  def printInfo(isl: IslandMap, board: GameBoard) {
    println("\n# Island global statistics")
    isl.stats match {
      case None =>
      case Some(d) => d.toSeq sortBy { _._1.toString  } foreach { case (stat, value) => println(s"  - $stat => $value") }
    }

    println("\n## Resources amounts")
    board.contents.toSeq.sortBy(_._2).reverse foreach { case (res, amount) => println(f"  - ${res}%-10s => $amount") }

    println("\n## Point of Interests available")
    board.pois foreach { case (loc, pois) => println(s"  - $loc: $pois") }

    if(board.startingTile.isDefined)
      println("\n  - Starting tile: " + board.startingTile)

    println("\n## Objectives")
    objectives foreach { case (res, amount) => println(f"  - ${res.name}%-10s: $amount") }

    exportPOIs(board,isl)

  }

  def exportPOIs(board: GameBoard, m: IslandMap) = {
    val viewer = PoiJSONViewer(board)
    viewer(m).renameTo(new File(s"$outputDir/_pois.json"))
  }

  type ChampResult = Iterable[Either[Result, (String, String)] with Product with Serializable]

  protected def run(g: Game, b:GameBoard, isl: IslandMap): ChampResult  = {
    players map { case (name, bot) =>
      try {
        Left(handlePlayer(name, bot, g, b, isl))
      } catch {
        case e: Error => Right((name, "Error : " + e.getClass.getCanonicalName))
        case e: Exception => Right((name, "Exception : " + e.getClass.getCanonicalName))
      }
    }
  }

  protected def handlePlayer(name: String, bot: IExplorerRaid, init: Game, board: GameBoard, m: IslandMap): Result = {
    val engine = new Engine(board, init, new Random(seed))
    val (events, game) = silent(engine,bot)
    val result = game.isOK match {
      case false => KO(name)
      case true => {
        val remaining = game.budget.remaining
        val men = game.crew.used
        val resources = game.collectedResources map { case (resource, data) => resource -> data }
        OK(name, remaining, men, resources.toSet)
      }
    }
    exportLog(name, events)
    exportVisitedMap(name, m, game, board.tileUnit, board)
    result
  }

  def silent(engine: Engine, bot: IExplorerRaid): (Seq[ExplorationEvent], Game) = {
    try {
      System.setOut(new PrintStream(new ByteArrayOutputStream()))
      System.setErr(new PrintStream(new ByteArrayOutputStream()))
      engine.run(bot)
    } finally {
      System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)))
      System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)))
    }
  }

  def exportLog(name: String, events: Seq[ExplorationEvent]): Unit = {
    val jsonEvents = events map { _.toJson } mkString("[", ",", "]")
    val writer = new PrintWriter(new File(s"$outputDir/$name.json"))
    try { writer.write(jsonEvents) } finally { writer.close() }
  }

  def exportVisitedMap(name: String, m: IslandMap, game: Game, tileUnit: Int, board: GameBoard): Boolean = {
    val pois = board.pois.values.flatten map { _.location } filter { _.isDefined } map { _.get } map { p => (p.x,p.y) }
    val fog = new FogOfWar(factor = tileUnit, visited = game.visited, scanned = game.scanned, pois = pois.toSet, size = m.size)
    val viewer = FogOfWarViewer(fog)
    viewer(m).renameTo(new File(s"$outputDir/$name.svg"))
  }

  def printResults(results: ChampResult): Unit = {
    val (left, right) = results partition { _.isLeft }
    val errors = (right map { _.right.get}).toSeq sortBy { _._1   }
    val (oks, kos) = left map { _.left.get } partition { _ match { case OK(_,_,_,_) => true; case _ => false } }

    if (oks.nonEmpty) {
      println("\n# Successful simulations")
      (oks map { _.asInstanceOf[OK] }).toSeq.sortBy { _.name } foreach { res =>
        println(s"\n## Playing bot delivered by ${res.name.toUpperCase}")
        println(s"  - Remaining budget: ${res.remaining}")
        println(s"  - Used men: ${res.men}")
        println(s"  - Collected resources:")
        if (res.resources.isEmpty)
          println("    - No resources collected")
        else
          res.resources foreach { r => println(s"    - ${r._1}: ${r._2}")}
      }
    }

    if(kos.nonEmpty) {
      println("\n# Simulation encountering gameplay issues \n")
      kos.toSeq.sortBy { _.name } foreach { r => println(s"  - ${r.name.toUpperCase}")}
    }

    if(errors.nonEmpty) {
      println("\n# Simulation throwing errors or exceptions \n")
      errors.toSeq.sortBy { _._1 } foreach { r => println(s"  - ${r._1.toUpperCase} => ${r._2}")}
    }
  }
}

trait Result { val name: String }
case class OK(override val name: String, remaining: Int, men: Int, resources: Set[(Resource, Int)]) extends Result
case class KO(override val name: String) extends Result