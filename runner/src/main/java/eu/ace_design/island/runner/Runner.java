package eu.ace_design.island.runner;

import eu.ace_design.island.arena.exporters.*;
import eu.ace_design.island.arena.utils.*;
import eu.ace_design.island.arena.utils.Result;
import eu.ace_design.island.bot.IExplorerRaid;
import eu.ace_design.island.game.*;
import eu.ace_design.island.io.IslandMapFactory;
import eu.ace_design.island.map.IslandMap;
import eu.ace_design.island.map.resources.Resource;
import eu.ace_design.island.stdlib.Resources;

import scala.Tuple2;
import scala.collection.JavaConversions$;

import java.io.File;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;


/**
 * The Runner class implements a Fluent API to configure and then fire an expedition on a given map
 */
public class Runner {

    public Runner(Class c) throws Exception {
        this.explorer = c;
    }

    /**
     * Fluent API to configure the runner
     */

	/**
	 * Instantiate a runner using an IExplorerRaid as playing entity
	 * @param c the class to instantiate
	 * @return  an instance of Runner
	 * @throws Exception
	 */
	public static Runner run(Class c) throws Exception {
		return new Runner(c);
	}

	/**
	 * Load a map stored in a JSON file
	 * @param jsonFile a File containing the map as a JSON data structure
	 * @return
	 */
	public Runner exploring(File jsonFile) {
		theIsland = IslandMapFactory.apply(jsonFile);
		return this;
	}

	/**
	 * Define the budget for this exploration
	 * @param budget
	 * @return
	 */
	public Runner backBefore(int budget) {
		this.budget = budget;
		return this;
	}


	/**
	 * Define the number of mens available for the raid
	 * @param nbMens
	 * @return
	 */
	public Runner withCrew(int nbMens) {
		this.nbMens = nbMens;
		return this;
	}

	/**
	 * Declare a contract to be fullfiled by the raiders
	 * @param amount  the expected amount of resources
	 * @param resource the resource to collect
	 * @return
	 */
	public Runner collecting(int amount, String resource) {
		Resource res = Resources.bindings().get(resource).get();
		this.contracts.put(res,amount);
		return this;
	}

	/**
	 * Define the seed used to initialize the random number generator
	 * @param l
	 * @return
	 */
	public Runner withSeed(Long l) {
		this.seed = l;
		return this;
	}

	/**
	 * Initial location and heading for the plane
	 * @param x x location (North -> South axis)
	 * @param y y location (West  -> East  axis)
	 * @param heading  direction
	 * @return
	 */
	public Runner startingAt(int x, int y, String heading) {
		this.thePlane = Plane$.MODULE$.apply(x,y, Directions.withName(heading));
		return this;
	}

	/**
	 * Output directory for the logs
	 * @param path
	 * @return
	 */
	public Runner storingInto(String path) {
		this.outputDir = new File(path);
		return this;
	}

	/**
	 * Timeout value, in milliseconds
	 * @param delay
	 * @return
	 */
	public Runner withTimeout(int delay) {
		this.timeoutDelay = delay;
		return this;
	}

    /**
     * Set the number of available creeks
     * @param creeks
     * @return
     */
    public Runner withCreeks(int creeks) {
        this.howManyCreeks = creeks;
        return this;
    }

    /**
     * Set the name of the island
     * @param name
     * @return
     */
    public Runner withName(String name) {
        this.name = name;
        return this;
    }

	/**
	 * Do not export map data to the disk
	 * @return
	 */
	public Runner silentMode() {
		this.exportMapData = false;
		return this;
	}

    /**
     * Do not export logs from a given run
     * @return
     */
    public Runner noLogs() {
        this.storeLogs = false;
        return this;
    }

    public Runner showReport() {
		this.displayReport = true;
		return this;
	}

    public Runner hideInfo() {
        this.displayInfo = false;
        return this;
    }

    /**
     * Starting the engine
     */

    public Result fire() {
        required();
        return process();
    }

    /**
     * Internal data structure and instance variables
     */

    private Class<? extends IExplorerRaid> explorer;
    private IslandMap theIsland;
    private Plane thePlane;
    private int budget;
    private int nbMens;
    private String name = "Lian_Yu";

    // Default values
    private Long seed = 0L;
    private int howManyCreeks = 10;
    private File outputDir = new File(".");
    private Map<Resource,Integer> contracts = new HashMap<>();
    private int timeoutDelay = Engine.DEFAULT_TIMEOUT_VALUE();
    private boolean displayReport = false;
	// By default we export everything
    private boolean exportMapData = true;
    private boolean storeLogs = true;
    private boolean displayInfo = true;


    /**
     * Wrapper from the Java DSL to the Arena Runner engine
     * @return
     */
    private Result process() {

        // 1. Creating the IslandData
        IslandData islandData = new IslandData(theIsland,seed,name);

        // 2. Creating the Contract
        java.util.Set<Tuple2<Resource,Object>> objectives = new java.util.HashSet<Tuple2<Resource,Object>>();
        for(Resource r: this.contracts.keySet()) {
            objectives.add(new Tuple2<Resource, Object>(r, this.contracts.get(r)));
        }
        Contract theContract = new Contract(nbMens,budget,thePlane,
                                         JavaConversions$.MODULE$.asScalaSet(objectives).toList().<Tuple2<Resource,Object>>toSet());

        // 3. Creating the Job
        Job theJob = new Job(islandData,theContract,howManyCreeks,timeoutDelay);

        // 4. Creating the player
        Player thePlayer = new Player(explorer.getSimpleName(),explorer);

        // 5. Initializing the arena in silent mode
        List<InfoDisplayer> displayers = new LinkedList<>();
        List<Class<?>> exporters = new LinkedList<>();

        // 6. Should we store the result log on the disk ?
        if ( storeLogs ) {
            exporters.add(GameLogExporter.class);
        }

        // 7. If the runner is not silent with respect to the map, adding the necessary exporters
        if ( exportMapData ) {
            exporters.add(POIsExporter.class);
            exporters.add(VisitedMapExporter.class);
        }

        // 8. If the runner should display stuff, add the needed displayers
        if ( displayInfo ) {
            displayers.add(ResourcesInfo$.MODULE$);
            displayers.add(ObjectiveInfo$.MODULE$);
			displayers.add(POIInfo$.MODULE$);
        }

        // 8. Creating the arena runner to be used
        eu.ace_design.island.arena.utils.Runner arenaRunner = new eu.ace_design.island.arena.utils.Runner(
                JavaConversions$.MODULE$.asScalaBuffer(displayers).toSeq(),
                JavaConversions$.MODULE$.asScalaBuffer(exporters).toSeq(),
                outputDir.getAbsolutePath());

        // 9. Running the runner
        Result result = arenaRunner.apply(thePlayer,theJob).head();

		if (isOk(result) && displayReport ) {
			OK ok = ((OK) result);
			if (ok.report().isDefined()) {
				System.out.println("Report: [" + ((OK) result).report().get() + "]");
			} else {
			 	System.out.println("Report: undefined");
			}
		}

        // 10. Returning the result
        return result;
    }

    // To enable assertion checking when encoutering an issue, add -ea to the VM arguments
    private void required() {
        assert explorer != null        : "Explorer cannot be null";
        assert theIsland != null       : "The island must be loaded";
        assert seed != null            : "Random seed generator must be provided"   ;
        assert thePlane != null        : "Plane initial location and heading must be provided";
        assert !(contracts.isEmpty())  : "Contracts cannot be empty" ;
        assert outputDir.exists()      : "Output directory must exist"    ;
        assert outputDir.isDirectory() : "Output directory must be a directory";
        assert outputDir.canWrite()    : "Output directory must be writable";
    }


    public static List<ExplorationEvent> exportEvents(Result r) {
        return JavaConversions$.MODULE$.seqAsJavaList(r.events());
    }

    public static boolean isOk(Result r) { return (r instanceof OK); }


}
