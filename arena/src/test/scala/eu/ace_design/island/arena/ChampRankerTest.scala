package eu.ace_design.island.arena

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
    KO("qaa"),
    OK("qab", 9474,  0, Set()),
    OK("qac", 161,   0, Set((WOOD, 2911))),
    OK("qad", 186,   0, Set()),
    OK("qba", 4403,  0, Set((FLOWER, 81))),
    OK("qbb", 80,    0, Set((WOOD, 39))),
    OK("qbc", 441,   0, Set((QUARTZ, 197),(WOOD, 1383),(FLOWER, 16))),
    OK("qbd", 13918, 0, Set()),
    KO("qbf"),
    OK("qca", 6438,  0, Set((QUARTZ, 127))),
    OK("qcb", 8752,  0, Set((FLOWER, 80))),
    OK("qcc", 7518,  0, Set((FLOWER, 37))),
    OK("qcd", 14994, 0, Set()),
    OK("qce", 115,   0, Set()),
    OK("qda", 5859,  0, Set((FLOWER, 80),(QUARTZ, 31),(WOOD, 3038))),
    OK("qdb", 8223,  0, Set((FLOWER, 80),(PLANK, 1930),(WOOD, 3004))),
    OK("qdc", 83,    0, Set()),
    OK("qdf", 14980, 0, Set())
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