package eu.ace_design.island.bot;


/**
 * This interface is used to model the interactions between a raid of explorer and the game engine
 *
 * The game engine acts as the following:
 *   1. instantiating an explorer raid without any parameters ( IExplorerRaid r = new MyRaid(); )
 *   2. describing the context using the initialize method ( r.initialize("..."); )
 *   3. while the explorer raid is taking valid decisions end until the end of the simulation:
 *     3.1. asking the raid to take a decision ( String decision = r.takeDecision(); )
 *     3.2 informing the raid about the results of its decisions ( r.acknowledgeResults("..."); )
 *
 * The game ends when:
 *   A) the raid take the decision to quit the island
 *   B) there is no more action point available
 *   C) an invalid decision is taken
 *
 * Option A is the only way to be considered in the ranking system. B and C are erroneous cases.
 *
 */
public interface IExplorerRaid {

	public void initialize(String context);

	public String takeDecision();

	public void acknowledgeResults(String results);

}
