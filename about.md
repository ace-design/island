---
layout: default
title:  "About!"
date:   2015-07-03
categories: info
---

# About the project!
 
* Author: [Sébastien Mosser](mailto:mosser@i3s.unice.fr) 
* Version: 2.0

Island is a programming game designed as a support for Software Engineering classes at the undergraduate level. 

It leverages [map generation techniques](http://www-cs-students.stanford.edu/~amitp/game-programming/polygon-map-generation/) to build the map of an Island. Thus, students can create their own **explorer bots** to discover and exploit the amazing resources available on the Island.

This game is developed at Université Nice-Sophia Antipolis, and used in the Polytech'Nice Sophia curriculum. A previous version of this game was used during Spring 2014, and the current version  takes into account feedback from both students and TA about the platform.

From a research point of view, it is also exploited to support the teaching of variability modelling at the graduate level. 

## Technical Information

Continuous integration is supported by [Drone.io](https://drone.io/github.com/ace-design/island/latest). Build is handled by [Maven](http://maven.apache.org/). The main engine is written in [Scala](http://www.scala-lang.org/), and tested on Java 8. Units tests are written using the [Specs2](http://etorreborre.github.io/specs2/) framework.


## Contributors

* Sébastien Mosser (Architecture, main implementation)
* Philippe Collet  (Variability)
* Antoine Pultier (Unity-based visualization mechanism)
* Pascal Tung (Web-based visualization mechanisms)

## References

* [Polygonal Map Generation for Games](http://www-cs-students.stanford.edu/~amitp/game-programming/polygon-map-generation/), Amit Patel


