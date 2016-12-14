package eu.ace_design.island.runner.sample;

import eu.ace_design.island.bot.IExplorerRaid;


public class FaultyBot implements IExplorerRaid {

	@Override
	public void initialize(String context) {}

	@Override
	public String takeDecision() {
        return "{ \"action\": \"stop\" }";
	}

	@Override
	public void acknowledgeResults(String results) { }

	@Override
	public String deliverFinalReport() {
		throw new RuntimeException("Ooops, something wetn wrong");
	}
}
