package eu.ace_design.island.runner;

import eu.ace_design.island.bot.IExplorerRaid;
import eu.ace_design.island.game.*;
import eu.ace_design.island.geom.Point;
import eu.ace_design.island.io.IslandMapFactory;
import eu.ace_design.island.map.IslandMap;
import eu.ace_design.island.map.resources.Resource;
import eu.ace_design.island.stdlib.POIGenerators;
import eu.ace_design.island.stdlib.Resources;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import eu.ace_design.island.viewer.svg.FogOfWar;
import eu.ace_design.island.viewer.svg.FogOfWarViewer;
import scala.Option;
import scala.Tuple2;
import scala.collection.JavaConversions$;
import scala.collection.immutable.*;
import scala.util.Random;


/**
 * The Runner class implements a Fluent API to configure and then fire an expedition on a given map
 */
public class Runner {

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
		this.thePlane = Plane$.MODULE$.apply(x,y,Directions.withName(heading));
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
	 * Trigger the game engine using the defined configuration
	 */
	public void fire() {
		required();
		Random rand = new Random(seed);
		Seq<POIGenerator> creeks =
				Nil$.MODULE$.$colon$colon((POIGenerator) new POIGenerators.WithCreeks(howManyCreeks)).toSeq();
		GameBoardBuilder builder =
				new GameBoardBuilder(package$.MODULE$.DEFAULT_TILE_UNIT(), creeks, rand);
		Option<Tuple2<Object,Object>> loc = Option.apply(thePlane.initial());
		GameBoard theBoard = builder.apply(theIsland);
		theBoard = theBoard.copy(theBoard.copy$default$1(),theBoard.copy$default$2(),
								 theBoard.copy$default$3(),theBoard.copy$default$4(),
								 theBoard.copy$default$5(), loc);
		java.util.Set<Tuple2<Resource,Object>> objs = new java.util.HashSet<Tuple2<Resource,Object>>();
		for(Resource r: this.contracts.keySet()) {
			objs.add(new Tuple2<Resource, Object>(r, this.contracts.get(r)));
		}
		Game theGame =
				Game$.MODULE$.apply(Budget$.MODULE$.apply(budget),
									Crew$.MODULE$.apply(nbMens),
									JavaConversions$.MODULE$.asScalaSet(objs).toList().<Tuple2<Resource,Object>>toSet());
		theGame = theGame.copy(theGame.copy$default$1(), theGame.copy$default$2(), theGame.copy$default$3(),
						 theGame.copy$default$4(), theGame.copy$default$5(),
						 Option.apply(thePlane),
				         theGame.copy$default$7(), theGame.copy$default$8(), theGame.copy$default$9(),
				         theGame.copy$default$10(), theGame.copy$default$11());

		startEngine(theGame, theBoard);
	}


	/**
	 * Internal data structure and instance variables
	 */

	private IExplorerRaid explorer;
	private IslandMap theIsland;
	private Plane thePlane;
	private int budget;
	private int nbMens;

	// Default values
	private Long seed = 0L;
	private int howManyCreeks = 10;
	private File outputDir = new File(".");
	private Map<Resource,Integer> contracts = new HashMap<Resource, Integer>();

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

	/**
	 * Start the engine, in silent mode (stdout and stderr are unavailable while playing)
	 * @param g
	 * @param b
	 */
	private void startEngine(Game g, GameBoard b) {
		Engine engine = new Engine(b,g, new Random(seed));
		Tuple2<scala.collection.Seq<ExplorationEvent>,Game> results = null;
		try {
			System.setOut(new PrintStream(new ByteArrayOutputStream()));
			System.setErr(new PrintStream(new ByteArrayOutputStream()));
			results = engine.run(explorer);
		} catch (Exception e) {
			System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)));
			System.err.println(e);
			e.printStackTrace();
		}
		finally {
			System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)));
			System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
		}
		if(results != null) {
			processResults(results._1(),results._2(), b);
		}
	}

	/**
	 * Process the results obtained at the end of the game
	 * @param events
	 * @param g
	 * @param b
	 */
	private void processResults(scala.collection.Seq<ExplorationEvent> events, Game g, GameBoard b) {
		if(g.isOK()) {
			System.out.println("Remaining budget: " + g.budget().remaining());
			System.out.println("Collected resources:");
			for(Resource r: JavaConversions$.MODULE$.asJavaCollection(g.collectedResources().keys())) {
				Object amount = g.collectedResources().get(r).get();
				System.out.println("  - " + r + ": " + amount);
			}
		} else {
			System.out.println("Game does not ended well :(");
		}
		exportLog(events);
		exportMap(g, b);
	}

	/**
	 * Generate the JSON event log
	 * @param events
	 */
	private void exportLog(scala.collection.Seq<ExplorationEvent> events) {
		System.out.println("Generating JSON log file");
		PrintWriter writer = null;
		try {
			writer = new PrintWriter(outputDir.getAbsolutePath() + "/log.json", "UTF-8");
			writer.print("[");
			for(ExplorationEvent evt: JavaConversions$.MODULE$.asJavaIterable(events)) {
				writer.print(evt.toJson() + ",");
			}
			writer.print("{}]");
		} catch(IOException ioe) {
			System.err.println("Unable to write JSON log file");
		} finally {
			writer.close();
		}
	}

	/**
	 * Generate a picture of the map
	 * @param g
	 * @param b
	 */
	private void exportMap(Game g, GameBoard b) {
		System.out.println("Generating SVG map file");
		java.util.List<PointOfInterest> all = new ArrayList<PointOfInterest>();
		for (Set<PointOfInterest> sp: JavaConversions$.MODULE$.asJavaCollection(b.pois().values())){
			all.addAll(JavaConversions$.MODULE$.asJavaCollection(sp));
		}
		java.util.Set<Tuple2<Object,Object>> tmp = new java.util.HashSet<Tuple2<Object,Object>>();
		for(PointOfInterest poi: all) {
			if(poi.location().isDefined()) {
				Point p = poi.location().get();
				tmp.add(new Tuple2<Object, Object>(p.x(),p.y()));
			}
		}
		scala.collection.immutable.Set<Tuple2<Object,Object>> pois = JavaConversions$.MODULE$.asScalaSet(tmp).toList().toSet();
		FogOfWar fog = new FogOfWar(package$.MODULE$.DEFAULT_TILE_UNIT(),g.visited(),g.scanned(),pois,theIsland.size());
		FogOfWarViewer viewer = new FogOfWarViewer(fog);
		viewer.apply(theIsland).renameTo(new File(outputDir.getPath() + "/map.svg"));
	}

	public Runner(Class c) throws Exception {
		this.explorer = (IExplorerRaid) c.newInstance();
	}

}
