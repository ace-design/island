package eu.ace_design.island.game

import eu.ace_design.island.map.resources.ExistingResources
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameBoardTest extends SpecificationWithJUnit {

  "GameBoardTest Specifications".title


  "A GameBoard" should {
    val empty = GameBoard(20)
    val contents = for(x <- 0 until 3; y <- 0 until 3) yield (x,y) -> (Tile() + Stock(ExistingResources.FISH, 10*x+y))
    val complete = (GameBoard(3) /: contents) { _ + _ }
    "be composed of tiles associated to locations" in {
      val bPrime = empty + ((0,0) -> Tile())
      bPrime.at(0,0) must_== Tile()
      empty.at(0,0) must throwAn[IllegalArgumentException]
    }
    "add a tile according to an update semantics" in {
      val t = Tile() + Stock(ExistingResources.FISH, 1)
      val bPrime = empty + ((0,0) -> Tile()) + ((0,0) -> t)
      bPrime.at(0,0) must_== t
    }
    "identify the available neighbors for a tile" in {
      complete.neighbors(0,0) must_== Set(Directions.SOUTH, Directions.EAST)
      complete.neighbors(1,1) must_== Directions.values
      complete.neighbors(-10,-10) must_== Set()
    }
    "identify the production of a given location" in {
      val local = complete + ((0,0) -> (Tile() + Stock(ExistingResources.FISH,10) + Stock(ExistingResources.ORE, 10)))
      local.produces(0,1) must_== Set(ExistingResources.FISH)
      local.produces(0,0) must_== Set(ExistingResources.FISH, ExistingResources.ORE)
    }
  }

  "A Tile" should {
    val t = Tile()
    "not produce resources by default" in { t.stock must beEmpty }
    "accept stock units added to its stock" in {
      val item = Stock(ExistingResources.FISH, 1)
      (t + item).stock must_== Set(item)
      t.stock must beEmpty
    }
    "reject the add of an already existing stock" in {
      val item = Stock(ExistingResources.FISH, 1)
      (t + item + item) must throwAn[IllegalArgumentException]
    }

  }

  "A Stock" should {
    "reject negative amounts" in {
      Stock(ExistingResources.FISH, -1) must throwAn[IllegalArgumentException]
    }
    "use an extraction factor of 1 by default" in {
      Stock(ExistingResources.FISH, 1).extraction must_== 1.0
    }
    "reject negative or null extraction factors" in {
      Stock(ExistingResources.FISH, amount = 1, extraction = 0) must throwAn[IllegalArgumentException]
      Stock(ExistingResources.FISH, amount = 1, extraction = -1) must throwAn[IllegalArgumentException]
    }
  }



}