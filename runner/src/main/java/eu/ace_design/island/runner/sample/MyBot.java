package eu.ace_design.island.runner.sample;

import eu.ace_design.island.bot.IExplorerRaid;


/**
 * Minimal viable product for an Explorer
 */
public class MyBot implements IExplorerRaid {

	public void initialize(String context) { }
	public String takeDecision() { return "{ \"action\": \"stop\" }"; }
	public void acknowledgeResults(String results) {}

}
