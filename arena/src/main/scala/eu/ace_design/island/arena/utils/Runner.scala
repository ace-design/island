package eu.ace_design.island.arena.utils

import java.io.{ByteArrayOutputStream, FileDescriptor, FileOutputStream, PrintStream}

import eu.ace_design.island.arena.exporters._
import eu.ace_design.island.game._
import eu.ace_design.island.map.resources.Resource
import eu.ace_design.island.stdlib.POIGenerators.{WithCreeks, WithEmergencySite}
import org.apache.logging.log4j.LogManager

import scala.util.Random

/**
  * The runner is used to run bots with respect to given jobs. Can be used as a functional object
  */
case class Runner(displayers: Seq[InfoDisplayer] = Seq(),
                  exporters: Seq[Class[_]] = Seq(),
                  outputDir: String = ".") {

  val _logger = LogManager.getLogger("eu.ace_design.Island/Arena")

  /**
    * Play a single player on a single job
    */
  def apply(player: Player, job: Job): Set[Result] = apply(Set(player), Set(job))

  /**
    * Play a set of players on a single job
    */
  def apply(players: Set[Player], job: Job): Set[Result] = apply(players, Set(job))

  /**
    * Play a set of bots with a set of jobs (each player plays all the jobs)
    */
  def apply(players: Set[Player], jobs: Set[Job]): Set[Result] = {
    // Jobs cannot be ran in // if the very same player is involved in multiple jobs.
    // The Teams trait should store Class instead of instances....
    jobs flatMap { job => process(job, players) }
  }

  /**
    * Process a job for a given set of players (only computing the game board once)
    */
  private def process(job: Job, players: Set[Player]): Set[Result] = {
    val random = new Random(job.islandData.seed)
    val builder = new GameBoardBuilder(poiGenerators = Seq(new WithCreeks(job.creeks), WithEmergencySite), rand = random)
    val theBoard: GameBoard = builder(job.islandData.island).copy(startingTile = Some(job.contract.plane.initial))
    val game = Game(Budget(job.contract.budget), Crew(job.contract.crew),
      job.contract.objectives).copy(plane = Some(job.contract.plane))

    displayers foreach { e => e(job.islandData.island, theBoard, game) }

    val results = players map { process(job, theBoard.copy(), game.copy(), _)}

    results
  }

  def process(job: Job, theBoard: GameBoard, game: Game, p: Player): Result = {
    _logger.info(s"Processing player [${p.name}] with island [${job.islandData.name}]")
    val start = System.currentTimeMillis()
    val r = try {
      val raw = play(p, new Engine(theBoard, game.copy(), new Random(job.islandData.seed), job.timeout))
      val result = raw._1.isOK match {
        case true  => OK(p.name, job.islandData.name, raw._3, raw._4, raw._2, raw._5)
        case false => KO(p.name, job.islandData.name, "game error", raw._2)
      }
      val exec = System.currentTimeMillis() - start;
      _logger.info(s" --> Execution time: ${exec}ms")
      if(exporters.contains(classOf[VisitedMapExporter])) {
        VisitedMapExporter(outputDir)(p.name, job.islandData.island, raw._1, theBoard.tileUnit, theBoard)
      }
      result.withExecTime(exec)
    } catch {
      case e: Exception => KO(p.name, job.islandData.name, e.getClass.getCanonicalName, null)
    }

    if(exporters.contains(classOf[GameLogExporter])) {
      GameLogExporter(outputDir)(s"${p.name}_${job.islandData.name}", r.events)
    }
    if(exporters.contains(classOf[POIsExporter])) {
      POIsExporter(outputDir)(theBoard,job.islandData.island)
    }
    r
  }

  /**
    * Play a given bot using a given engine, silently redirecting the stdout andf stderr standard exits
    */
  private def play(player: Player, engine: Engine):
        (Game, Seq[ExplorationEvent], Integer, Set[(Resource, Int)], Option[String]) = {
    val dataset = try {
      System.setOut(new PrintStream(new ByteArrayOutputStream()))
      System.setErr(new PrintStream(new ByteArrayOutputStream()))
      engine.run(player.bot.newInstance())
    } finally {
      System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)))
      System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)))
    }
    val game = dataset._2
    val report = dataset._3
    (game, dataset._1, game.budget.remaining, game.collectedResources.toSet filter { case (res, i) => i > 0 }, report)
  }


}
