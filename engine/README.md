Island
======

* Author: Sébastien Mosser [mosser AT i3s DOT unice DOT fr]
* Version: 0.1

Island is a programming game designed as a support for Software Engineering classes at the undergraduate level. 

It leverages [map generation techniques](http://www-cs-students.stanford.edu/~amitp/game-programming/polygon-map-generation/) to build the map of an Island. Thus, students can create their own **explorer bots** to discover and exploit the amazing resources available on the Island.

This game is developed at Université Nice-Sophia Antipolis, and used in the Polytech'Nice Sophia curriculum. From a research point of view, it is also exploited to support the teaching of variability modelling at the graduate level. 

#### Technical Information

Continuous integration is supported by [Drone.io](https://drone.io/github.com/ace-design/island/latest). Build is handled by [Maven](http://maven.apache.org/). The main engine is written in [Scala](http://www.scala-lang.org/), and tested on Java 8. Units tests are written using the [Specs2](http://etorreborre.github.io/specs2/) framework.

#### Compiling the engine

Simply `clone` the git repository, and use `mvn package` to build the JAR file. The `Main.scala` file shows several example for map generation.

#### Contributors

* Sébastien Mosser (Architecture, main implementation)
* Philippe Collet  (Variability)

#### References, sources

* [Polygonal Map Generation for Games](http://www-cs-students.stanford.edu/~amitp/game-programming/polygon-map-generation/)
* [Génération procédurale de cartes #1](http://linuxfr.org/news/je-cree-mon-jeu-video-e10-generation-procedurale-de-carte-partie-1) (FR)
* [Génération procédurale de cartes #2](http://linuxfr.org/news/je-cree-mon-jeu-video-e11-generation-procedurale-de-carte-partie-2) (FR)
