package eu.ace_design.island.map.processes

import eu.ace_design.island.map._

class AssignElevationTest extends ProcessTestTrait {

  import ElevationFunctions._

  "AssignElevationTest Specifications".title

  override val preconditions: IslandMap => IslandMap = { m =>
    val donuts = DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
    MinimalDistanceToCoast(
      IdentifyCoastLine(
        IdentifyLakesAndOcean(
          AlignVertexWaterBasedOnFaces(
            IdentifyWaterArea(donuts, 30)(IdentifyBorders(m))))))
  }
  val elev = linear(10) _
  override val processUnderTest = AssignElevation(mapper = distance, elevator = elev)

  "The AssignElevation process" should {

    val coastline = result.findVerticesWith(Set(IsCoast())) map { p => result.vertexRef(p) }
    import ExistingWaterKind._
    val raw = result.findFacesWith(Set(WaterKind(OCEAN))) flatMap { f => result.cornerRefs(f) + f.center }
    val oceans = raw diff coastline // taking all the vertices involved in oceans, removing coastline
    val lakes = result.findFacesWith(Set(WaterKind(LAKE))) flatMap { f => result.cornerRefs(f) + f.center }

    "not annotate ocean vertices with elevation annotation" in {
      oceans foreach { result.vertexProps.isAnnotatedAs(_, HasForHeight()) must beFalse }
      true must beTrue // glitch to allow implicit conversion (thus compilation). real test is above.
    }
    "give an elevation >= 0 to any land vertex (!ocean, !lake) " in {
      val land = result.vertexRefs diff oceans diff lakes
      land must not be empty
      land foreach { l =>
        result.vertexProps.isAnnotatedAs(l, HasForHeight()) must beTrue
        result.vertexProps.getValue(l, HasForHeight()) must beGreaterThanOrEqualTo(0.0)
      }
      true must beTrue
    }
    "assign elevations to lakes (including centers)" in {
      lakes foreach { result.vertexProps.isAnnotatedAs(_, HasForHeight()) must beTrue }
      lakes must not(beEmpty)
    }
  }

  "The ElevationFunctions library" should {
    val vertices = 0 until 100

    "define the linear function" in {
      val result = ElevationFunctions.linear(100)(vertices)

      result.toSet must_== (for(v <- vertices) yield (v -> v)).toMap
    }

    /*"define the peak function that re-scale the elevation" in {
      val summit = 2706 // Lets build the "Monte Cinto", Corsica highest mountain
      val result = ElevationFunctions.peak(summit)(distances)
      result foreach { case (k,v) =>
        distances.get(k) must beSome
        v must beLessThanOrEqualTo(summit.toDouble)
      }
      result must haveValue(summit.toDouble)
      result must haveSize(distances.size)
    }

    "define the redistribute function to redistribute the elevation based on a cumulative function" in {
      val factor = 0.5 ; val max = distances.values.max
      val result = ElevationFunctions.redistribute(factor)(distances)
      result foreach { case (k,v) =>
        distances.get(k) must beSome
        v must beLessThanOrEqualTo(factor * max)
      }
      result must haveSize(distances.size)
    }    */

  }

}