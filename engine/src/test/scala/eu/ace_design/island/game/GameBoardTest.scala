package eu.ace_design.island.game

import eu.ace_design.island.stdlib.Resources
import eu.ace_design.island.stdlib.Biomes._
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameBoardTest extends SpecificationWithJUnit {

  "GameBoardTest Specifications".title

  import Resources._

  "A GameBoard" should {
    val empty = GameBoard(20, null)
    val contents = for(x <- 0 until 3; y <- 0 until 3) yield (x,y) -> (Tile() + Stock(FISH, 10*x+y))
    val complete = (GameBoard(3, null) /: contents) { _ + _ }

    "be composed of tiles associated to locations" in {
      val bPrime = empty + ((0,0) -> Tile())
      bPrime.at(0,0) must_== Tile()
      empty.at(0,0) must throwAn[IllegalArgumentException]
    }
    "add a tile according to an update semantics" in {
      val t = Tile() + Stock(FISH, 1)
      val bPrime = empty + ((0,0) -> Tile()) + ((0,0) -> t)
      bPrime.at(0,0) must_== t
    }

    "identify the production of a given location" in {
      val local = complete + ((0,0) -> (Tile() + Stock(FISH,10) + Stock(ORE, 10)))
      local.produces(0,1) must_== Set(FISH)
      local.produces(0,0) must_== Set(FISH, ORE)
    }
    "support the add of PointOfInterests" in {
      import eu.ace_design.island.stdlib.PointOfInterests.Creek
      complete.getPOIs(0, 0) must_== Set()
      val p1 = new Creek("aPort #1", None);
      val p2 = new Creek("aPort #2", None)
      complete addPOI ((120, 250) -> p1) must throwAn[IllegalArgumentException]
      val up = complete addPOI ((0, 0) -> p1) addPOI ((0, 0) -> p2) addPOI ((0, 1) -> p1)
      complete.getPOIs(0, 0) must_== Set()
      complete.getPOIs(0, 1) must_== Set()
      up.getPOIs(0, 0) must_== Set(p1, p2)
      up.getPOIs(0, 1) must_== Set(p1)
    }
    "find point of interests by kind" in {
      import eu.ace_design.island.stdlib.PointOfInterests._
      val c1 = Creek("c1", None); val c2 = Creek("c2", None)
      val h = Hideout("h", None)
      val up = complete addPOI ((0, 0) -> c1) addPOI ((0, 0) -> h) addPOI ((0, 1) -> c2)
      up.findPOIsByType(Creek(null,null)) must_== Set((0,0) -> c1, (0,1) -> c2)
      up.findPOIsByType(Hideout(null,null)) must_== Set((0,0) -> h)
    }

    "compute the pitch factor to move from one tile to another one" in {
      val mocked = empty + ((0,0) -> Tile(altitude = 100.0)) + ((0,1) -> Tile(altitude = 100.0))  +
                           ((1,0) -> Tile(altitude = 150.0)) + ((1,1) -> Tile(altitude = 0.0))   +
                           ((0,2) -> Tile(altitude = 350.0))
      mocked.pitchFactor((0,0), (0,0)) must_== 1.0    // flat, same loc
      mocked.pitchFactor((0,0), (0,1)) must_== 1.0    // flat, locs ≠
      mocked.pitchFactor((0,0), (1,0)) must beGreaterThan(1.0) // going up
      mocked.pitchFactor((0,0), (1,1)) must beGreaterThan(1.0) // going down
      mocked.pitchFactor((0,0), (0,2)) must beGreaterThan(mocked.pitchFactor((0,0), (1,0)))  // Comparison
      mocked.pitchFactor((0,0), (1,1)) must beGreaterThan(mocked.pitchFactor((0,0), (1,0)))
      // reflexivity
      mocked.pitchFactor((0,0), (0,0)) must_==  mocked.pitchFactor((0,0), (0,0))
      mocked.pitchFactor((0,0), (0,1)) must_==  mocked.pitchFactor((0,1), (0,0))
      mocked.pitchFactor((0,0), (1,0)) must_==  mocked.pitchFactor((1,0), (0,0))
      mocked.pitchFactor((0,0), (1,1)) must_==  mocked.pitchFactor((1,1), (0,0))
      mocked.pitchFactor((0,0), (0,2)) must_==  mocked.pitchFactor((0,2), (0,0))
    }

    "compute the biome factor (cost of crossing the current biomes)" in {
      val b = empty + ((0,0) -> Tile(biomes = Set((BEACH, 0.2), (MANGROVE, 0.8))))
      b.biomeFactor((0,0)) must_== (0.2 * BEACH.crossFactor) + (0.8 * MANGROVE.crossFactor)
    }


  }

  "A Tile" should {

    val t = Tile()
    "not produce resources by default" in { t.stock must beEmpty }
    "accept stock units added to its stock" in {
      val item = Stock(FISH, 1)
      (t + item).stock must_== Set(item)
      t.stock must beEmpty
    }
    "ignore stock with empty contents" in {
      (t + Stock(FISH, 0)) must_== t
      (t + Stock(FISH, 1)) must_!= t
    }
    "reject the add of an already existing stock" in {
      val item = Stock(FISH, 1)
      (t + item + item) must throwAn[IllegalArgumentException]
    }
    "reject the addition of a set of heterogeneous resource" in {
      t ++ Set(Stock(FISH, 1), Stock(ORE, 1)) must throwAn[IllegalArgumentException]
    }
    "support the addition of a set of homogeneous resource" in {
      val tile = t + Stock(FISH, 1) ++ Set(Stock(ORE, 100, 1.0), Stock(ORE, 50, 2.0))
      (tile ++ Set()) must_== tile

      val s = tile.stock.find( _.resource == ORE)
      s must beSome
      s.get.amount must_== 150
      s.get.extraction must_== 1.5
    }

    "support the addition of bulk stock units" in {
      val stocks = Set(Stock(FISH, 1), Stock(ORE, 100, 1.0), Stock(ORE, 50, 2.0))
      val tile = t bulkAdd stocks
      tile.resources must_== Set(FISH, ORE)
      tile.stock.find( _.resource == ORE).get.amount must_== 150
      tile.stock.find( _.resource == ORE).get.extraction must_== 1.5
      tile.stock.find( _.resource == FISH).get.amount must_== 1
      tile.stock.find( _.resource == FISH).get.extraction must_== 1.0
    }

  }

  "A Stock" should {
    "reject negative amounts" in {
      Stock(FISH, -1) must throwAn[IllegalArgumentException]
    }
    "use an extraction factor of 1 by default" in {
      Stock(FISH, 1).extraction must_== 1.0
    }
    "reject negative or null extraction factors" in {
      Stock(FISH, amount = 1, extraction = 0) must throwAn[IllegalArgumentException]
      Stock(FISH, amount = 1, extraction = -1) must throwAn[IllegalArgumentException]
    }
  }



}