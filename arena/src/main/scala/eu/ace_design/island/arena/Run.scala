package eu.ace_design.island.arena

import java.io.File

import eu.ace_design.island.arena.exporters._
import eu.ace_design.island.arena.rankers.ChampRanker
import eu.ace_design.island.arena.utils.{Contract, IslandData, Job, Runner}
import eu.ace_design.island.dsl.DiSLand
import eu.ace_design.island.game.Plane
import eu.ace_design.island.io.IslandMapFactory
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.map.resources.Resource
import eu.ace_design.island.arena.utils._


trait Run extends Teams with DiSLand {

  def number: String
  def budget: Int
  def crew: Int
  def plane: Plane
  def theIsland: IslandMap
  def seed: Long
  def objectives: Set[(Resource, Int)]

  def outputDir = s"championships/week${number}"
  lazy val islFile = new File(s"$outputDir/_map.json")

  def generate(): Unit = {
    val out = new File(outputDir)
    if (! out.exists())
      out.mkdirs()
    IslandMapFactory(theIsland, islFile)
    println(s"  - Seed: [0x${theIsland.uuid.get.toHexString.toUpperCase}L]")
    theIsland -> ( s"$outputDir/_map" as pdf)
    theIsland -> ( s"$outputDir/_map" as svg)
  }

  final def run(): Unit = {
    if (!islFile.exists()) { generate() }

    val islandData = IslandData(island = IslandMapFactory(islFile), seed = seed, name = s"week${number}")
    val contract = Contract(crew = crew, budget = budget, plane = plane, objectives = objectives)
    val job = Job(islandData, contract)

    val runner = Runner(Seq(MapInfo, IslandStatistics, POIInfo, ResourcesInfo, ObjectiveInfo),
                        Seq(classOf[VisitedMapExporter], classOf[GameLogExporter]), outputDir)

    println(s"\n## Starting a Run with the following players on island week$number")
    println(s"  - $playerNames\n")

    val results = runner(asPlayers, job)

    // Displaying results
    printResults(results, objectives)
  }



  def printResults(results: Set[Result], objectives: Set[(Resource, Int)]): Unit = {
    val (oks, kos) = results partition { _.isInstanceOf[OK] }

    rank(oks, objectives)

    if (oks.nonEmpty) {
      println("\n# Successful simulations")
      (oks map { _.asInstanceOf[OK] }).toSeq.sortBy { _.name } foreach { res =>
        println(s"\n## Playing bot delivered by ${res.name.toUpperCase}")
        println(s"  - Remaining budget: ${res.remaining}")
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

  }

  def rank(comingBak: Iterable[Result], objectives: Set[(Resource, Int)]): Unit = {
    val ranker = new ChampRanker(objectives)
    val ranked = ranker(comingBak)
    if(comingBak.nonEmpty) {
      println("\n# Automatic Ranking")
      ranked foreach { r =>
        println(s"  - ${r.name.toUpperCase}: ${r.contracts.mkString("[",", ","]")}, ${r.budget} action points left")
      }
    }
  }


  lazy val asJob: Job = {
    val islData = IslandData(island = theIsland, seed = seed, name = s"week$number")
    val contract = Contract(crew = crew, budget = budget, plane = plane, objectives = objectives)
    Job(islandData = islData, contract = contract)
  }

}
