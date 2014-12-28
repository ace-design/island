package eu.ace_design.island.map.resources

import org.specs2.mutable.SpecificationWithJUnit

/**
 * This file is part of the ${PROJECT_NAME} project
 * @author mosser (26/12/2014, 11:52)
 **/
class WhittakerDiagramsTest extends SpecificationWithJUnit {

  import eu.ace_design.island.map.resources.WhittakerDiagrams.complete
  import eu.ace_design.island.map.resources.ExistingBiomes.{LAKE, GLACIER}

  "WhittakerDiagramsTest Specifications".title

  "The complete diagram" should {

    "return a glacier when a lake is located above 1300m" in {
      complete freshWater 129.9  must_== LAKE
      complete freshWater 130   must_== GLACIER
    }

    "reject non-relevant values" in {
      complete(moisture = -1, elevation = 20) must throwAn[IllegalArgumentException]
      complete(moisture = 101, elevation = 20) must throwAn[IllegalArgumentException]
      complete(moisture = 50, elevation = -1) must throwAn[IllegalArgumentException]
    }
  }

  "The parser" should {
    val template =
      """
        |@ice_level 600
        |#    0%  10% 20% 30% 40% 50% %60 70% 80% 90%
        |-    ALP ALP ALP ALP ALP SNO SNO SNO SNO SNO
        |1100 teD teD GRA GRA teF teF teF teF teF teF
        | 300 STD GRA GRA GRA trS trS teF trF trF trF
        |  25 BEA BEA BEA BEA BEA trF trF trF MAN MAN
      """.stripMargin

    "reject unknown biome code" in {
      val errorneous = template.replaceAll("GRA", "GRR")
      (WhittakerParser(errorneous)) must throwAn[IllegalArgumentException]
    }

    val parsed = WhittakerParser(template)

    "Accept a template as input" in {
      parsed must haveInterface[WhittakerDiagram]
    }

    "build relevant diagrams" in {
      parsed.iceLevel must_== 600.0
      parsed(0.0,0.0) must_== ExistingBiomes.BEACH
      parsed(33.33, 15.0) must_== ExistingBiomes.GRASSLAND
      parsed(12.2, 220.0) must_== ExistingBiomes.ALPINE
      parsed.biomes must_== Set(ExistingBiomes.ALPINE, ExistingBiomes.SNOW, ExistingBiomes.TEMPERATE_DESERT,
                                ExistingBiomes.GRASSLAND, ExistingBiomes.TEMPERATE_DECIDUOUS_FOREST,
                                ExistingBiomes.SUB_TROPICAL_DESERT, ExistingBiomes.TROPICAL_SEASONAL_FOREST,
                                ExistingBiomes.TROPICAL_RAIN_FOREST, ExistingBiomes.BEACH, ExistingBiomes.MANGROVE)
    }

    "reject non-relevant values" in {
      parsed(moisture = -1, elevation = 20)  must throwAn[IllegalArgumentException]
      parsed(moisture = 101, elevation = 20) must throwAn[IllegalArgumentException]
      parsed(moisture = 50, elevation = -1)  must throwAn[IllegalArgumentException]
    }
  }
}
