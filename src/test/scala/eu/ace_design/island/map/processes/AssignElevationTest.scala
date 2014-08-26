package eu.ace_design.island.map.processes

import eu.ace_design.island.map._
import org.specs2.mutable._

class AssignElevationTest extends SpecificationWithJUnit {

  "AssignElevationTest Specifications".title


  "The AssignElevation process" should {
    val preconditions: IslandMap => IslandMap = { m =>
      val donuts = DiskShape(SIZE, SIZE.toDouble / 2 * 0.8)// DonutShape(SIZE, SIZE.toDouble / 2 * 0.8, SIZE.toDouble / 2 * 0.2)
      MinimalDistanceToCoast(
        IdentifyCoastLine(
          IdentifyLakesAndOcean(
            AlignVertexWaterBasedOnFaces(
              IdentifyWaterArea(donuts, 30)(IdentifyBorders(m))))))
    }
    val updated =  AssignElevation(ElevationFunctions.identity)(preconditions(entry))

    val coastline = updated.findVerticesWith(Set(IsCoast())) map { p => updated.vertexRef(p) }
    import ExistingWaterKind._
    val raw = updated.findFacesWith(Set(WaterKind(OCEAN))) flatMap { f => updated.cornerRefs(f) + f.center }
    val oceans = raw diff coastline // taking all the vertices involved in oceans, removing coastline

    "not annotate ocean vertices with elevation annotation" in {
      oceans foreach { updated.vertexProps.isAnnotatedAs(_, HasForHeight()) must beFalse }
      true must beTrue // glitch to allow implicit conversion (thus compilation). real test is above.
    }
    "give an elevation >= 0 to any vertex that is not in the ocean " in {
      val land = updated.vertexRefs diff oceans
      land must not be empty
      land foreach { l =>
        updated.vertexProps.isAnnotatedAs(l, HasForHeight()) must beTrue
        updated.vertexProps.getValue(l, HasForHeight()) must beGreaterThanOrEqualTo(0.0)
      }
      true must beTrue
    }
  }

  "The ElevationFunctions library" should {
    val distances = (for(i <- 0 until 100) yield i -> i*2.0).toMap

    "define the identity function" in {
      val result = ElevationFunctions.identity(distances)
      result.toSet must_== distances.toSet
    }

    "define the peak function that re-scale the elevation" in {
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
    }

  }

}