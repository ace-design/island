---
layout: default
title: Creating a Bot
---

# Running the game engine

Island comes with an _experimental_ embedded language that allows developers to run explorers outside of the global game arena.

## Maven Dependency

The language is provided in the `island-runner` artifact:

```xml
    <dependency>
      <groupId>eu.ace-design</groupId>
      <artifactId>island-runner</artifactId>
      <version>1.0-SNAPSHOT</version>
    </dependency>
```

The runner dependency is hosted by the Sonatype OSS repository, like the player one. As a consequence, the same `repository` entry must be defined in the POM.


## Starting a run

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

# Visualizing a run (FR)

Thanks to [Günther Jungbluth](gunther.jungbluth.poirier@gmail.com), a visualization engine was developed using Javascript (original idea from Pascal Tung). The engine is hosted on github pages: [http://ace-design.github.io/IslandExploration/](http://ace-design.github.io/IslandExploration/)
