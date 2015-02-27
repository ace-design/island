package eu.ace_design.island.util

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TimeoutTest extends SpecificationWithJUnit with Timeout {

  "Timeout Specifications".title

  def sleeping(seconds: Int): String = {
    Thread.sleep(seconds*1000)
    "beauty"
  }

  "The timeout environment" should {
    "execute the given code when in the timebox" in {
      timeout(2000) { sleeping(1) } must_== "beauty"
    }
    "throw a TimeoutException when the given code exceeds the timebox" in {
      timeout(500) { sleeping(1) } must throwA[java.util.concurrent.TimeoutException]
    }
  }

}