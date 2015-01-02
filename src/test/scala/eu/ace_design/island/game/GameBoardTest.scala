package eu.ace_design.island.game

import eu.ace_design.island.map.resources.ExistingResources
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameBoardTest extends SpecificationWithJUnit {

  "GameBoardTest Specifications".title

  import ExistingResources._

  "A GameBoard" should {
    val empty = GameBoard(20)
    val contents = for(x <- 0 until 3; y <- 0 until 3) yield (x,y) -> (Tile() + Stock(FISH, 10*x+y))
    val complete = (GameBoard(3) /: contents) { _ + _ }
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
    "identify the available neighbors for a tile" in {
      complete.neighbors(0,0) must_== Set(Directions.SOUTH, Directions.EAST)
      complete.neighbors(1,1) must_== Directions.values
      complete.neighbors(-10,-10) must_== Set()
    }
    "identify the production of a given location" in {
      val local = complete + ((0,0) -> (Tile() + Stock(FISH,10) + Stock(ORE, 10)))
      local.produces(0,1) must_== Set(FISH)
      local.produces(0,0) must_== Set(FISH, ORE)
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