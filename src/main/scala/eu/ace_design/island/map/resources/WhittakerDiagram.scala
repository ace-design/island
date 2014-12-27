package eu.ace_design.island.map.resources

import eu.ace_design.island.map.resources.ExistingBiomes.Biome


object ExistingBiomes extends Enumeration {
  type Biome = Value
  val  ALPINE, SNOW, BEACH, TROPICAL_RAIN_FOREST, MANGROVE, TUNDRA, GRASSLAND, TROPICAL_SEASONAL_FOREST,
  TEMPERATE_DESERT, TAIGA, SUB_TROPICAL_DESERT, TEMPERATE_RAIN_FOREST, SHRUBLAND, TEMPERATE_DECIDUOUS_FOREST,
  OCEAN, LAKE, GLACIER = Value

  private val _binding: Map[String, Value] = Map(
    "ALP" -> ALPINE, "SNO" -> SNOW,      "BEA" -> BEACH, "MAN" -> MANGROVE, "TUN" -> TUNDRA, "GRA" -> GRASSLAND,
    "TAI" -> TAIGA,  "SHR" -> SHRUBLAND, "OCE" -> OCEAN, "LAK" -> LAKE,     "GLA" -> GLACIER,
    "STD" -> SUB_TROPICAL_DESERT,   "trF" -> TROPICAL_RAIN_FOREST, "trS" -> TROPICAL_SEASONAL_FOREST,
    "teR" -> TEMPERATE_RAIN_FOREST, "teD" -> TEMPERATE_DESERT,     "teF" -> TEMPERATE_DECIDUOUS_FOREST
  )

  def apply(key: String): Value = _binding(key)

}

/**
 * In the literature, a Whittaker diagram is used to distribute the different biomes with respect to temperature and
 * precipitation level. We use here a gross approximation, considering that the elevation is an approximation of the
 * temperature (higher elevations are colder than lowest one), and the soil moisture for the precipitation level (as
 * there is no precipitation concept in the Island's model)
 *
 * The diagram defines 2 operations:
 *   - for water faces, freshWater is used to decide if a fresh water reservoir is a lake or a glacier
 *   - for land faces,  apply takes as input an elevation and a moisture, and returns the associated biome.
 *
 * A diagram is not designed to address ocean faces.
 *
 */
trait WhittakerDiagram {
  import ExistingBiomes.{Biome, GLACIER, LAKE}

  /**
   * Compute the biome for a land faces
   * @param moisture moisture level of the face
   * @param elevation elevation of the face
   * @return on of the ExistingBiomes to be associated to this face (excepting GLACIER, OCEAN and LAKE)
   */
  def apply(moisture: Double, elevation: Double): Biome = {
    require(moisture >= 0.0 && moisture <= 100.0, "Moisture level must be in [0,100]")
    require(elevation >= 0.0, "Elevation level cannot be negative")
    assign(moisture, elevation)
  }

  /**
   * Compute the biome for a freshwater face
   * @param elevation elevation of the face, in px (should use pixelFactor to find the ~ in meters)
   * @return GLACIER or LAKE w.r.t. the elevation
   */
  def freshWater(elevation: Double): Biome = if (elevation * PIXEL_FACTOR >= iceLevel) GLACIER else LAKE

  // define the elevation where freshwater becomes a glacier
  val iceLevel: Double

  // implementation of the whittaker diagram, as a function.
  protected def assign(moisture: Double, elevation: Double): Biome

}

/**
 * The parser loads whittaker-like diagrams encoded as plain text (see tests for examples)
 */
object WhittakerParser {

  /**
   * A Diagram loaded from a given template. Private class as only the WhittakerDiagram Trait is relevant
   * @param tpl the template (encoded in a String) used to model the diagram
   */
  private case class TemplateDiagram(tpl: String) extends WhittakerDiagram {

    // Removing empty lines and lines starting with a # from template
    private val data = tpl.split("\\n") map { _.trim } filterNot { _ == "" } filterNot { _.startsWith("#") }

    // The ice level is identified by a single line in the template starting with @ice_level
    override  val iceLevel: Double = data find { _.startsWith("@ice_level") } match {
        case None    => throw new IllegalArgumentException("Ice level is not properly available in template")
        case Some(l) => (l split "\\s")(1).toFloat
    }

    // Load the contents of the template into a data structure
    private val _internal: Map[Int, Seq[Biome]] = try {
      val (lasts, others) = data filterNot { _.startsWith("@") } partition {  _.startsWith("-") }
      (others map { line =>
        val l = clean(line)
        l(0).toInt -> (l.tail map { ExistingBiomes(_) }).toSeq
      }).toMap + (Int.MaxValue -> (clean(lasts.head).tail map { ExistingBiomes(_) }).toSeq)
    } catch {
      case e: Exception => throw new IllegalArgumentException("Something went wrong while parsing the diagram" + e)
    }

    private def clean(l: String): Array[String] = l.split("\\s") filterNot { _ == "" }

    // implementation of the whittaker diagram, as a function.
    override protected def assign(moisture: Double, elevation: Double): Biome = {
      val e = (elevation * PIXEL_FACTOR).toInt // Transforming the elevation, from pixels to meters
      val key = (_internal.keys filter { _ > e }).min
      val m = (moisture / 10).toInt
      _internal(key)(m)
    }
  }

  def apply(tpl: String): WhittakerDiagram = TemplateDiagram(tpl)
}

/**
 * a Library containing off-the-shelf Whittaker diagrams
 */
object WhittakerDiagrams {

  lazy val complete: WhittakerDiagram = WhittakerParser(completeTemplate)


  /**
   * Templates available to represents different ecosystems
   * Syntax is the following:
   *
   */

  private val completeTemplate =
    """
      |@ice_level 1300
      |#    0%  10% 20% 30% 40% 50% %60 70% 80% 90%
      |-    ALP ALP ALP ALP ALP SNO SNO SNO SNO SNO
      |2100 ALP ALP TUN TUN TUN TUN TAI TAI SNO SNO
      |1900 ALP TUN TUN TUN TUN TUN TAI TAI TAI SNO
      |1700 teD TUN TUN SHR SHR SHR TAI TAI TAI TAI
      |1500 teD teD teD SHR SHR SHR teF TAI TAI TAI
      |1300 teD teD SHR SHR SHR SHR teF teF TAI TAI
      |1100 teD teD GRA GRA teF teF teF teF teF teF
      | 900 teD teD GRA GRA GRA GRA teF teF teF teR
      | 700 teD GRA GRA GRA teF teF teF teF teR teR
      | 500 teD GRA GRA GRA trS trS teR teR teR teR
      | 300 STD GRA GRA GRA trS trS teF trF trF trF
      | 100 STD STD GRA trS trS trS trS trF trF trF
      |  50 STD STD STD trS trS trF trF trF trF MAN
      |  25 BEA BEA BEA BEA BEA trF trF trF MAN MAN
    """.stripMargin
}
