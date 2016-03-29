package eu.ace_design.island.arena

import eu.ace_design.island.arena.utils.{KO, OK, Result}
import eu.ace_design.island.arena.rankers.ChampRanker
import eu.ace_design.island.map.resources.Resource
import eu.ace_design.island.stdlib.Resources._
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ChampRankerTest extends SpecificationWithJUnit {

  "ChampRankerTest Specifications".title

  // Based on QGL championship, week #1
  val objectives: Set[(Resource, Int)] = Set((FLOWER, 80), (WOOD, 3000), (QUARTZ, 800), (PLANK, 1000))
  val results: Set[Result] = Set(
    KO("qaa", "island", "??"),
    OK("qab", "island", 9474,  Set()),
    OK("qac", "island", 161,   Set((WOOD, 2911))),
    OK("qad", "island", 186,   Set()),
    OK("qba", "island", 4403,  Set((FLOWER, 81))),
    OK("qbb", "island", 80,    Set((WOOD, 39))),
    OK("qbc", "island", 441,   Set((QUARTZ, 197),(WOOD, 1383),(FLOWER, 16))),
    OK("qbd", "island", 13918, Set()),
    KO("qbf", "island", "??"),
    OK("qca", "island", 6438,  Set((QUARTZ, 127))),
    OK("qcb", "island", 8752,  Set((FLOWER, 80))),
    OK("qcc", "island", 7518,  Set((FLOWER, 37))),
    OK("qcd", "island", 14994, Set()),
    OK("qce", "island", 115,   Set()),
    OK("qda", "island", 5859,  Set((FLOWER, 80),(QUARTZ, 31),(WOOD, 3038))),
    OK("qdb", "island", 8223,  Set((FLOWER, 80),(PLANK, 1930),(WOOD, 3004))),
    OK("qdc", "island", 83,    Set()),
    OK("qdf", "island", 14980, Set())
  )


  "The championship ranker system" should {
    import ChampRanker._

    val ranker = new ChampRanker(objectives)
    val ranked = ranker(results)

    "return ranked results" in { ranked must not be empty }

    "reject KO's teams" in {
      ranked must have size 16
    }

    "support contract verification" in {
      ranker.fulfilled(Set())              must beEqualTo(Set())
      ranker.fulfilled(Set((FUR, 1000)))   must beEqualTo(Set())
      ranker.fulfilled(Set((FLOWER, 100))) must beEqualTo(Set(FLOWER))
      ranker.fulfilled(Set((FLOWER, 100), (FUR, 100)))  must beEqualTo(Set(FLOWER))
      ranker.fulfilled(Set((FLOWER, 100), (WOOD,4000))) must beEqualTo(Set(FLOWER, WOOD))
      ranker.fulfilled(Set((FLOWER, 80),(PLANK, 1930),(WOOD, 3004))) must beEqualTo(Set(FLOWER, WOOD, PLANK))
    }

    "assign points to fulfilled contracts" in {
      fulfilled2points(Set())             must_== 0
      fulfilled2points(Set(FLOWER))       must_== 1
      fulfilled2points(Set(PLANK))        must_== 2
      fulfilled2points(Set(FLOWER, WOOD)) must_== 2
      fulfilled2points(Set(FUR,PLANK))    must_== 3
    }

    "consider that a primary resource worth half a transformed one" in {
      ChampRanker.TRANSFORMED_POINT  must_== 2 * ChampRanker.PRIMARY_POINT
      resource2points(PLANK)  must_== ChampRanker.TRANSFORMED_POINT
      resource2points(FLOWER) must_== ChampRanker.PRIMARY_POINT
    }

    "order ranks thanks to fulfilled contracts" in {
      ranked.head.name must_== "qdb"
      ranked(1).name   must_== "qda"
    }

    "order ranks thanks to remaining budget when contract fulfillment equality" in {
      ranked(2).name must_== "qcb"
      ranked(3).name must_== "qba"
    }

  }

}