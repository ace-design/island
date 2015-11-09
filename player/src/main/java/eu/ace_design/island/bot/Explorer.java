package eu.ace_design.island.bot;

/**
 * This file is part of the island project
 *
 * @author mosser (08/11/2015, 14:03)
 **/
public class Explorer implements IExplorerRaid {

	public void initialize(String context) {
		return;
	}

	public String takeDecision() {
		String action = "{ \"action\": \"stop\" }";
		return action;
	}

	public void acknowledgeResults(String results) {
		return;
	}
}
