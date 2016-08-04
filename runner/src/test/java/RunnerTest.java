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
		File outputMap = new File(out.toAbsolutePath().toString() + "/MyBot.svg");
		run(out);
		assertTrue(outputMap.exists());
		long d1 = outputMap.lastModified();
		run(out);
		long d2 = outputMap.lastModified();
		assertTrue(d2 > d1);
	}

	@Test
	public void testSystemOutRestored() throws Exception {
        PrintStream old = System.out;
        try {
            Path outDir = Files.createTempDirectory("test-sysout-restore");
            System.out.println("Output directory is: " + outDir.toString());
            File out = new File(outDir.toAbsolutePath().toString() + "/out.txt");
            PrintStream ps = new PrintStream(out);
            System.setOut(ps);

            run(outDir);

            // the output stream was created
            assertTrue(out.exists());
            // the output stream is empty (the engine has redirected sysout to /dev/null)
            assertTrue(out.length() == 0);
            // the output stream is now different than the one we set (cannot check if equals to stdout)
            assertNotEquals(ps, System.out);
        } finally {
            // In any case, resetting the stream to te initial one.
            System.setOut(old);
        }
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
                .hideInfo()
				.fire();
	}

	private static File load(String resFile)  {
		return new File(Main.class.getClassLoader().getResource(resFile).getFile());
	}

}
