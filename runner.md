---
layout: default
title: Creating a Bot
---

# Running the game engine

## Visualizing a run 

Thanks to [Günther Jungbluth](gunther.jungbluth.poirier@gmail.com), a visualization engine was developed using Javascript (original idea from Pascal Tung). The visualization engine is hosted on github pages: [http://ace-design.github.io/IslandExploration/](http://ace-design.github.io/IslandExploration/). One simply upload a log file obtained from the engine (_i.e._, the map, the exploration log and the location of the creeks).

## Using the Game Runner
Island comes with an _experimental_ embedded language that allows developers to run explorers outside of the global game arena.

### Maven Dependency

The language is provided in the `island-runner` artifact:

```xml
    <dependency>
      <groupId>eu.ace-design</groupId>
      <artifactId>island-runner</artifactId>
      <version>2.0-SNAPSHOT</version>
    </dependency>
```

The runner dependency is hosted by the Sonatype OSS repository, like the player one. As a consequence, the same `repository` entry must be defined in the POM.

### Starting a run

One can implement a run in a simple Java application. For example, here is how one can start the engine using the `MyBot` player:

```java
import java.io.File;
import static eu.ace_design.island.runner.Runner.*;

public class Main {

  public static void main(String[] args) throws Exception {

    run(MyBot.class)
        .exploring(new File("map.json"))
        .withSeed(0L)
        .startingAt(1, 1, "EAST")
        .backBefore(7000)
        .withCrew(15)
        .collecting(1000, "WOOD")
        .collecting(300,  "QUARTZ")
        .collecting(10,   "FLOWER")
        .storingInto("./outputs")
        .fire();
		 
	}
}
```

The previous code is a classical Java application. It instantiates an explorer raid based on the class `MyBot`, and set some parameters:

  * the file containing the map to explore
  * the random seed generator to use
  * the initial location of the plane
  * the given deadline (action points)
  * the number of mens available in this raid
  * three contracts
  * the output directory (it must exist)

The `fire` method collects all the previously given information, and triggers the engine with this configuration.

## Using the Game Arena

The arena allows one to run multiple players on the very same map, ensuring that all the bots share the very same exploitation condition (_e.g._, random seed generation for the map exploration).

### Maven dependency

The game arena is provided as a maven dependency:

```xml
<dependency>
	<groupId>eu.ace-design</groupId>
	<artifactId>island-arena</artifactId>
   <version>2.0-SNAPSHOT</version>
</dependency>
```

The arena dependency is hosted by the Sonatype OSS repository, like the player one. As a consequence, the same `repository` entry must be defined in the POM.

### Starting a Championship

A championship is a simple Scala object, mixing two traits:

  1. The `Run` traits that automates the championship process
  2. A trait extending the `Player` one that declares the available players for this championship

Here is an example of a championship started at the University of Nice:

```scala
object Week47 extends Run with SI3 {

  override val number: String = "47"

  override val seed: Long                = Islands.s47
  override lazy val theIsland: IslandMap = Islands.week47

  override val crew: Int      = 15
  override val budget: Int    = 7000
  override val plane: Plane   = Plane(1,1,Directions.EAST)
  override val objectives: Set[(Resource, Int)] = Set((WOOD, 1000), (QUARTZ, 300), (FLOWER,10))

  override def players = all - "qad" - "qcb" - "qcc" - "qce" - "qcf"

  run()

}
```

The `players` line allows one to control which players will play among the declared one (one can decide to disqualify a player for external reasons).

Here is an excerpt of the definition of the `SI3` trait, that model a set of player for this championship:

```scala
trait SI3 extends Teams {

  def all: Map[String, IExplorerRaid] = g1 ++ g2 ++ g3 ++ g4

  def players = all

  private lazy val g1: Map[String, IExplorerRaid] = Map(
    "qaa" -> new fr.unice.polytech.qgl.qaa.Explorer()
    //...
  }
  
  // ...
}
```