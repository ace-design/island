package eu.ace_design.island.game

import eu.ace_design.island.bot.IExplorerRaid
import org.specs2.mutable._
import org.specs2.mock.Mockito
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineTest extends SpecificationWithJUnit with Mockito {

  "EngineTest Specifications".title



  "the stoppingExplorer mock" should {
    import MockedExplorers.stoppingExplorer
    "support initialization" in {
      stoppingExplorer.initialize("") must not(throwAn[Exception])
    }
    "always stop when asked to take a decision" in {
      stoppingExplorer.takeDecision() must_== """{ "action": "stop" }"""
      stoppingExplorer.takeDecision() must_== """{ "action": "stop" }"""
    }
    "support result acknowledgment" in {
      stoppingExplorer.acknowledgeResults("") must not(throwAn[Exception])
    }
  }


  object MockedExplorers {
    val stoppingExplorer =  mock[IExplorerRaid]
    stoppingExplorer.takeDecision() returns """{ "action": "stop" }"""
  }


}
