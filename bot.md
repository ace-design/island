---
layout: default
title: Creating a Bot
---


# Bot description

The Island game is played by a bot, _i.e._, an automated robot that will implement the player behavior. In that sense, Island is a [_programming game_](https://en.wikipedia.org/wiki/Programming_game), where the player actually designs and implements the champion that will play in the arena on her behalf instead of playing herself.

The bot designed by the player must provide an empty constructor and implement the `IExplorerRaid` interface. These constraints allow the game engine to create an instance of the bot and communicate with it through simple method invocations. The interface defines three methods the bot must implements:

  * `initialize`: This method is invoked right after the initialization;
  * `takeDecision`: This method is invoked each time the bot must decide which action must be played;
  * `acknowledgeResults`: This method is invoked right after the previous one, to provide the results of the action when applied to the world.

**As a player, your only responsibility is to implement this interface**. The game engine interacts with the bot thanks to the exchange of messages, _i.e._ the context of the game, the actions taken by the bot and the associated results. These messages are implemented as JSON elements, and described in the other pages of this website:

  * [Available actions]({{ site.baseurl }}actions)
  * [Game description]()


The algorithm used by the game engine is the following, in pseudo-java:

```java
IExplorerRaid raid = new MyExplorer();        // Empty Constructor

String context = "{ ... }";raid.initialize(context);                     // Context initialization
while ( !endOfGame ) {  String decision = raid.takeDecision();      // Decision taking, one at a time  String result  = engine.compute(decision);  raid.acknowledgeResults(result);	          // Result storage}

String report = raid.deliverFinalReport();    // Retrieve report from the raid
```

The `endOfGame` condition is defined as the following (only the last one is correct from the championship point of view):

  1. The bot ran out of budget;
  2. The bot took a decision that disqualify its participation;
  3. The bot answers `stop` when the `takeDecision` method is invoked, with enough budget to come back.


# Implementing a bot

The engine, the championship arena and the Bots are implemented as maven artefacts. As a consequence, there is no need to install anything on your computer, excepting maven to handle the dependencies installation.

## Maven dependency

The `IExplorerRaid` interface is published as a Maven artefact, thanks to the open source repositories provided by Sonatype. As a consequence, your `pom.xml` file must contain: _(i)_ a dependency to the `island-player` artefact and _(ii)_ the declaration of the open source repositories.

```xml
<!-- ... -->
<repositories>
  <repository>
    <id>oss.sonatype.org-snapshots</id>
    <name>snapshots</name>
    <url>http://oss.sonatype.org/content/repositories/snapshots</url>
  </repository>
  <!-- ... -->
</repositories>

<!-- ... -->

<dependencies>
  <!-- ... -->
  <dependency>
    <groupId>eu.ace-design</groupId>
    <artifactId>island-player</artifactId>
    <version>2.0-SNAPSHOT</version>
  </dependency>
</dependencies>
<!-- ... -->
```

## Package Isolation

Your bot must not interfere with the bots defined by the other players that will play in the same championship. Thus, your bot must be defined in a dedicated package, and all the classes you are using must be defined inside this package.

**Polytech students:** You must be using the following package pattern: `fr.unice.polytech.si3.qgl.xxxx`, where `xxxx` is your project key identifier on _mjøllnir_. Your bot must be implemented in a class named `Explorer` located in this package.

## Minimal Viable Product (MVP)

The minimal bot for the Island game is a bot that will simply stop the game when invoked. Here is the associated implementation:

```java
package eu.ace_design.island.mvp;

// A minimal bot, with minimal documentation
public class Explorer implements IExplorerRaid {

  // we do not care of the context in this minimal implementation
  @Override
  public void initialize(String context) {
    return;
  }

  // we always return the same action: stopping the game
  @Override
  public String takeDecision() {
    return "{ \"action\": \"stop\" }";
  }

  // we do not care of the result of the action in this minimal implementation
  @Override
  public void acknowledgeResults(String results) {
    return;
  }
  
  // Dumb final report
  @Override
  public String deliverFinalReport() {
    return "Hei Boss, here is my report!";
  }
	
}
```

