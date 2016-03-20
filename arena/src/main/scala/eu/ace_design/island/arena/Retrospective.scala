package eu.ace_design.island.arena

import java.io.{FileDescriptor, FileOutputStream, ByteArrayOutputStream, PrintStream}

import eu.ace_design.island.bot.IExplorerRaid
import eu.ace_design.island.game._
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.map.resources.Resource
import eu.ace_design.island.stdlib.POIGenerators.WithCreeks

import scala.util.Random

object Retrospective {

  /************************************************************
   ** Data structures necessary to handle retrospective runs **
   ************************************************************/

  case class Player(name: String, bot: IExplorerRaid)

  case class Job(islandData: IslandData, contract: Contract)
  case class IslandData(island: IslandMap, seed: Long, name: String)
  case class Contract(crew: Int, budget: Int, plane: Plane, objectives: Set[(Resource, Int)])

  case class Result(playerName: String, islandName: String, remaining: Integer, resources: Set[(Resource, Int)])


  /**
    * Supporting the run of a given retrospective
    */
  object Runner {

    def apply(players: Set[Player], jobs: Set[Job]): Set[Result] = {
      jobs flatMap { job => process(job, players) }
    }

    private def process(job: Job, players: Set[Player]): Set[Result] = {
      val random = new Random(job.islandData.seed)
      val builder = new GameBoardBuilder(poiGenerators = Seq(new WithCreeks(10)), rand = random)
      val theBoard: GameBoard = builder(job.islandData.island).copy(startingTile = Some(job.contract.plane.initial))
      val game = Game(Budget(job.contract.budget), Crew(job.contract.crew),
        job.contract.objectives).copy(plane = Some(job.contract.plane))

      players map { p => {
        val raw = play(p, new Engine(theBoard, game.copy(), new Random(job.islandData.seed)))
        Result(p.name, job.islandData.name, raw._1, raw._2)
      }
      }
    }

    private def play(player: Player, engine: Engine): (Integer, Set[(Resource, Int)]) = {
      val dataset = try {
        System.setOut(new PrintStream(new ByteArrayOutputStream()))
        System.setErr(new PrintStream(new ByteArrayOutputStream()))
        engine.run(player.bot)
      } finally {
        System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)))
        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)))
      }
      val game = dataset._2
      (game.budget.remaining, game.collectedResources.toSet filter { case (res, i) => i > 0 })
    }

  }


  object Displayer {

    def exportToMarkdown(results: Set[Result], contracts: Map[String, Set[(Resource, Int)]]) {
      println("# Retrospective Results\n")
      val grouped = results groupBy { res => res.playerName }
      grouped foreach { case (player, result) => {
        println(s"## $player\n")
        result.toSeq sortBy { _.islandName } foreach { r => {
          println(s"  - ${r.islandName}:  [remaining: ${r.remaining}]")
          if(!r.resources.isEmpty) { println(s"    - ${r.resources mkString " "} ") }
        }}
      }}
    }

    def exportToCSV(result: Set[Result]) {

    }


  }

}
