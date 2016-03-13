package eu.ace_design.island.arena

import eu.ace_design.island.dsl.DiSLand
import eu.ace_design.island.game._
import eu.ace_design.island.io.IslandMapFactory
import eu.ace_design.island.map.IslandMap

import java.io.File
import scala.util.Random

import eu.ace_design.island.stdlib.POIGenerators.WithCreeks

trait Run extends Championship with DiSLand {

  val number: String
  val budget: Int
  val crew: Int
  val plane: Plane
  val theIsland: IslandMap

  override def outputDir = s"championships/week${number}/"
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
    if(!islFile.exists()) { generate() }

    val island =  IslandMapFactory(islFile)
    val random = new Random(seed)
    val builder = new GameBoardBuilder(poiGenerators = Seq(new WithCreeks(10)), rand = random)

    val theBoard: GameBoard = builder(island).copy(startingTile = Some(plane.initial))
    printInfo(island, theBoard)
    // Building the game engine and the associated objectives
    val initialization = Game(Budget(budget), Crew(crew), objectives).copy(plane = Some(plane))
    println("\n## Running championship with the following players")
    println(s"  - $playerNames")
    val results = run(initialization, theBoard, island)
    // Displaying results
    printResults(results, objectives)
  }

}
