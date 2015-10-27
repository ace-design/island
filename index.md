---
layout: default
title: Homepage
---

# The project

_Software Engineering_ takes an important part in the course portfolio offered by [Polytech'Nice Sophia](http://informatique.polytechnice.fr/) (part of [Université Nice Sophia-Antipolis](http://unice.fr/)) computer science curriculums (at both graduate and undergraduate level). 

When the course _Tools for Software Engineering_ was refactored in 2012, we were looking for a lab project that will support project development with tools such a _version control systems_, _build automation_, and _unit tests_. After 2 years experimenting with a board game named [Myrmes](https://boardgamegeek.com/boardgame/126792/myrmes), we obtain the following specifications for this lab project:

  1. __Competition triggers commitment__. By asking student teams to create bots acting like Myrmes players and running a championship on a weekly basis, we obtained very good projects from students, with a continuous amelioration of the deliveries after each championship;
  2. __Keep it simple__. Myrmes was _almost_ too complicated by nature. The main issue with Myrmes was the interactions that exists between rules. It was a real overhead for a single semester project. We need a game with an understandable rule set, and a clear scope definition of each action available during the game;
  3. __Surprise them__! Myrmes was too predictable and deterministic. It was almost possible to hardcode a single strategy and play always the same actions. We need something less predictable.

Based on theses 3 hypothesis, the **Island** programming game was designed during the hot summer months of 2014, and experimented in its initial version with undergraduate students during Spring'15.

## Principles

Players are designing bots that control an exploration operation. The goal of the operation is to explore an unknown island, retrieving the ressources its sponsor asked for. The operation has a given time to fulfill the expected contract, expressed as an amount of _action points_ (each action taken during the exploration consumes action points). The exploration is a twofold process:

  1. __Air phase__: Using a reconnaissance drone, the player identifies relevant part of the **Island**, for example creeks where a boat can land, coastline, ...;
  2. __Terrestrial phase__: when enough information about the island is collected, the player sends an exploration team by boat. The team will then exploit the resources available on the island or create manufactured resources (_e.g._, rum made from sugar cane). 

The ultimate goal of the player is to optimize the exploration, in order to collect enough resources on the Island and be back at the home port before the end of the stopwatch. 

## Galery

Here are some examples of Islands one can generate and explore using this software.

<div align="center">
  <img src="{{ site.baseurl }}assets/galery-portrait.png" />
</div>


  
  
