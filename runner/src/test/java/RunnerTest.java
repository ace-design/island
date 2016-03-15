import eu.ace_design.island.runner.Runner;
import eu.ace_design.island.runner.sample.MyBot;
import org.junit.Test;
import java.io.File;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.Assert.*;

public class RunnerTest {


	@Test
	public void testMapSVGReplacement() throws Exception {
		Path out = Files.createTempDirectory("test-map-replace");
		File outputMap = new File(out.toAbsolutePath().toString() + "/map.svg");
		run(out);
		assertTrue(outputMap.exists());
		long d1 = outputMap.lastModified();
		run(out);
		long d2 = outputMap.lastModified();
		assertTrue(d2 > d1);
	}

	@Test
	public void testSystemOutRestored() throws Exception {
		Path out = Files.createTempDirectory("test-sysout-restore");
		File outputTxt = new File(out.toAbsolutePath().toString() + "/log.txt");
		PrintStream old = System.out;
		PrintStream ps = new PrintStream(outputTxt);
		System.out.println("Setting System.out to file "+outputTxt);
		System.setOut(ps);
		run(out);

		assertTrue(outputTxt.exists());

		assertEquals(ps,System.out);
		System.setOut(old);
	}
	private static void run(Path out) throws Exception {
		Runner.run(MyBot.class)
				.exploring(load("map.json"))   // A File containing a map as a JSON object
				.withSeed(0L)
				.startingAt(1, 1, "EAST")
				.backBefore(7000)
				.withCrew(15)
				.collecting(1000, "WOOD")
				.storingInto(out.toAbsolutePath().toString())
				.fire();
	}

	private static File load(String resFile)  {
		return new File(Main.class.getClassLoader().getResource(resFile).getFile());
	}

}
