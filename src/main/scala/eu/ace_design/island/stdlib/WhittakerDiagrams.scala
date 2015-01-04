package eu.ace_design.island.stdlib

import eu.ace_design.island.map.resources.{WhittakerParser, WhittakerDiagram}

/**
 * a Library containing off-the-shelf Whittaker diagrams
 */
object WhittakerDiagrams {

  lazy val caribbean: WhittakerDiagram = WhittakerParser(caribbeanTemplate)
  lazy val nordic: WhittakerDiagram    = WhittakerParser(nordicTemplate)
  lazy val complete: WhittakerDiagram  = WhittakerParser(completeTemplate)

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

  private val caribbeanTemplate =
  """
    |@ice_level 1600
    |#    0%  10% 20% 30% 40% 50% %60 70% 80% 90%
    |-    SHR SHR trS trS trS trS trS trS trF trF
    |150  SHR SHR SHR SHR trS trS trS trF trF trF
    |50   SHR SHR SHR trS trS trF trF trF trF trF
    |20   SHR SHR BEA BEA BEA BEA MAN MAN MAN MAN
  """.stripMargin

  private val nordicTemplate =
    """
      |@ice_level 900
      |#    0%  10% 20% 30% 40% 50% %60 70% 80% 90%
      |-    ALP ALP ALP ALP ALP SNO SNO SNO SNO SNO
      |800  ALP ALP TUN TUN TUN TUN TAI TAI TAI TAI
      |400  TUN TUN TUN TUN teF teF teF teF TAI TAI
      |150  GRA TUN TUN teF teF teF teF teF teF TAI
      |50   GRA GRA teF teF teF teF teF teF teF teF
      |15   GRA GRA BEA BEA BEA BEA BEA teF teF teF
    """.stripMargin

}
