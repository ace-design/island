package eu.ace_design.island.io

import eu.ace_design.island.map._
import eu.ace_design.island.map.resources.{Soils, Conditions}
import org.json.JSONObject
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PropertySetFactoryTest extends SpecificationWithJUnit {

  "PropertySetFactoryTest Specifications".title


  "The JSON property set factory" should {
    val empty = PropertySet()
    val pSet = empty bulkAdd(Set(1), IsWater()) bulkAdd(Set(1,2), IsCoast()) bulkAdd(Set(1,2,3), IsBorder())

    "Return the empty array for the empty property set" in {
      PropertySetFactory(empty).length() must_== 0
    }

    "Serialize the right number of elements" in {
      val json = PropertySetFactory(pSet)
      for(i <- 0 until json.length) {
        val elem = json.getJSONObject(i)
        val oracle = elem.getInt("key") match {
          case 1 => 3 // 1 is annotated as IsWater, IsCoast and IsBorder
          case 2 => 2 // 2 is annotated as IsCoast and IsBorder
          case 3 => 1 // 3 is annotated as IsBorder
        }
        elem.getJSONArray("vals").length must_== oracle
      }
      json.length() must_== pSet.size
    }

    "be bidirectional" in {
      PropertySetFactory(PropertySetFactory(empty)) must_== empty
      PropertySetFactory(PropertySetFactory(pSet)) must_== pSet
    }

  }


  "The JSON property factory" should {

    val f = PropertyFactory

    "reject unknown key" in {
      val json = new JSONObject().put("p","unknown").put("v","???")
      f(json) must throwAn[IllegalArgumentException]
    }

    "support IsBorder" in {
      IsBorder(true)  must_== f(f(IsBorder(true)))
      IsBorder(false) must_== f(f(IsBorder(false)))
    }

    "support IsWater" in {
      IsWater(true)  must_== f(f(IsWater(true)))
      IsWater(false) must_== f(f(IsWater(false)))
    }

    "support IsCoast" in {
      IsCoast(true)  must_== f(f(IsCoast(true)))
      IsCoast(false) must_== f(f(IsCoast(false)))
    }

    "support RiverFlow" in {
      RiverFlow(0)  must_== f(f(RiverFlow(0)))
      RiverFlow(10) must_== f(f(RiverFlow(10)))
    }

    "support DistanceToCoast" in {
      DistanceToCoast(0.0) must_== f(f(DistanceToCoast(0.0)))
      DistanceToCoast(1.1) must_== f(f(DistanceToCoast(1.1)))
    }

    "support HasForHeight" in {
      HasForHeight(0.0) must_== f(f(HasForHeight(0.0)))
      HasForHeight(1.1) must_== f(f(HasForHeight(1.1)))
    }

    "support HasForMoisture" in {
      HasForMoisture(0.0) must_== f(f(HasForMoisture(0.0)))
      HasForMoisture(1.1) must_== f(f(HasForMoisture(1.1)))
    }

    "support HasforArea" in {
      HasForArea(0.0) must_== f(f(HasForArea(0.0)))
      HasForArea(1.1) must_== f(f(HasForArea(1.1)))
    }

    "support HasForPitch" in {
      HasForPitch(0.0) must_== f(f(HasForPitch(0.0)))
      HasForPitch(1.1) must_== f(f(HasForPitch(1.1)))
    }

    "support WaterKind" in {
      val str = """ { "p": "waterKind", "v": "LAKE" }"""
      f(str) must_== WaterKind(ExistingWaterKind.LAKE)
      WaterKind(ExistingWaterKind.LAKE)  must_== f(f(WaterKind(ExistingWaterKind.LAKE)))
      WaterKind(ExistingWaterKind.OCEAN) must_== f(f(WaterKind(ExistingWaterKind.OCEAN)))
    }

    "support HasForCondition" in {
      HasForCondition(Conditions.EASY)  must_== f(f(HasForCondition(Conditions.EASY)))
      HasForCondition(Conditions.FAIR)  must_== f(f(HasForCondition(Conditions.FAIR)))
      HasForCondition(Conditions.HARSH) must_== f(f(HasForCondition(Conditions.HARSH)))
    }

    "support HasForSoil" in {
      HasForSoil(Soils.FERTILE) must_== f(f(HasForSoil(Soils.FERTILE)))
      HasForSoil(Soils.NORMAL)  must_== f(f(HasForSoil(Soils.NORMAL)))
      HasForSoil(Soils.POOR)    must_== f(f(HasForSoil(Soils.POOR)))
    }

    "support HasForBiome" in {
      HasForBiome() must_== f(f(HasForBiome()))
    }
  }
}