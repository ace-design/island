package eu.ace_design.island.runner.sample;

import eu.ace_design.island.bot.IExplorerRaid;


/**
 * Minimal viable product for an Explorer
 */
public class MyBot implements IExplorerRaid {

	@Override
	public void initialize(String context) {
		System.out.println("Here goes the initialization code");
	}

	@Override
	public String takeDecision() {
        System.out.println("Bravely deciding to stop");
        return "{ \"action\": \"stop\" }";
	}

	@Override
	public void acknowledgeResults(String results) {
        System.out.println("Acknowledging the result of my brave decision");
    }

	@Override
	public String deliverFinalReport() {
		return "Hei Boss, here is my report!";
	}
}
