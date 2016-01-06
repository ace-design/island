package eu.ace_design.island.runner.sample;

import java.io.*;
import static eu.ace_design.island.runner.Runner.*;

public class Main {

	public static void main(String[] args) throws Exception {

		run(MyBot.class)
				.exploring(load("map.json"))   // A File containing a map as a JSON object
				.withSeed(0L)
				.startingAt(1, 1, "EAST")
				.backBefore(7000)
				.withCrew(15)
				.collecting(1000, "WOOD")
				.collecting(300,  "QUARTZ")
				.collecting(10,   "FLOWER")
				.storingInto("./outputs")      // The output directory must exists
				.withTimeout(2000)             // player timeout is 2 seconds
				.fire();

	}

	private static File load(String resFile)  {
		return new File(Main.class.getClassLoader().getResource(resFile).getFile());
	}

}
