---
layout: default
title:  "Install Instructions"
date:   2015-07-06
categories: info
---

# How to install Island

Island is JVM-based. Mainly written in Scala, it relies on state-of-practice tools available in the Java ecosystem. The following options are available to install Island on your system:

  * Using Maven, automatically downloading the published version on Sonatype OSS
  * Using Maven, after compiling the latest version on your computer
  * Using an "all-in-one" jar, provided with all the dependencies used by the 
  
The project requires Java 7+, and maven 3.  

## Using Maven

Island is hosted on Sonatype OSS repository, as a regular maven dependencies. Thus, any project relying on maven for build and dependency management can benefit from it for (almost) free.

First, in your POM, You need to declare in your `repositories` the Sonatype OSS snapshot reprository

    <repository>
        <id>oss.sonatype.org-snapshots</id>
        <name>snapshots</name>
        <url>http://oss.sonatype.org/content/repositories/snapshots</url>
    </repository>

Then, add in your dependencies the following dependency to obtain the island generator engine: 

    <dependency>
      <groupId>eu.ace-design</groupId>
      <artifactId>island-engine</artifactId>
      <version>1.0-SNAPSHOT</version>
    </dependency>


## Compiling the latest version

The source code of Islan is available on GitHub: [ace-design/Island](https://github.com/ace-design/island). 

The process is quite simple using a terminal: _(i)_ clone the repository on your local computer, and _(ii)_ run maven to build the program and install it on your local repository. The process is independent of your operating system, and only requires a Java installation and Maven 3.

    mosser@azrael: ~$ git clone https://github.com/ace-design/island.git
    mosser@azrael: ~$ cd island
    mosser@azrael: ~/island$ mvn clean install
    _..._
    mosser@azrael: ~/island$ exit
    
**Warning**: The build using Maven can be **quite** long the first time you run it, basically because maven will download all the needed dependencies and store them locally on your computer. A **good** connection to the internet will help this process.  

## Using the "all-in-one" jar (experimental)

A Jar version that contains all the dependencies of Island can be built from . 

