package eu.ace_design.island.viewer

import java.awt.Color

/**
 * Color Palettes extracted from Cynthia Brewer colorbrewer awesome tool (http://colorbrewer2.org/)
 */
object ColorBrewer {

  // Classical colors (not from Brewer's palettes)
  final val BLACK = new Color(0,   0,   0)
  final val WHITE = new Color(255, 255, 255)
  final val BROWN = new Color(133, 127, 48)

  // Grays are defined with the 3-class Greys palette [http://colorbrewer2.org/?type=sequential&scheme=Greys&n=3]
  final val LIGHT_GREY  = new Color(240, 240, 240)
  final val MEDIUM_GREY = new Color(189, 189, 189)
  final val DARK_GREY   = new Color(99,  99,  99 )

  // Blues are defined with the 9-class PuBu palette [http://colorbrewer2.org/?type=sequential&scheme=PuBu&n=9]
  final val DARK_BLUE   = new Color(2,   56, 88 ) // 3-class PuBu theme  #9
  final val MEDIUM_BLUE = new Color(4,   90, 141) // 3-class PuBu theme  #8
  final val LIGHT_BLUE  = new Color(5,  112, 176) // 3-class PuBu theme  #7

  // Yellows are defined with the 6-class YlOrBr palette [http://colorbrewer2.org/?type=sequential&scheme=YlOrBr&n=6]
  final val LIGHT_YELLOW  = new Color(255, 255, 212) // 6-class YlOrBr theme #1
  final val MEDIUM_YELLOW = new Color(254, 227, 145) // 6-class YlOrBr theme #2
  final val DARK_YELLOW   = new Color(254, 198, 79 ) // 6-class YlOrBr theme #3

  // Oranges are defined with the 6-class YlOrBr palette [http://colorbrewer2.org/?type=sequential&scheme=YlOrBr&n=6]
  final val LIGHT_ORANGE  = new Color(254, 153, 41 ) // 6-class YlOrBr theme #4
  final val MEDIUM_ORANGE = new Color(217, 95 , 14 ) // 6-class YlOrBr theme #5
  final val DARK_ORANGE   = new Color(153, 52,  4  ) // 6-class YlOrBr theme #6

  // Greens are defined with the 9-class Greens palette [http://colorbrewer2.org/?type=sequential&scheme=Greens&n=9]
  final val ULTRA_DARK_GREEN  = new Color(0,   68,  27 ) // 9-class Greens #9
  final val DARK_GREEN        = new Color(0,   109, 44 ) // 9-class Greens #8
  final val MEDIUM_GREEN      = new Color(35,  139, 69 ) // 9-class Greens #7
  final val LIGHT_GREEN       = new Color(116, 198, 118) // 9-class Greens #5
  final val ULTRA_LIGHT_GREEN = new Color(161, 217, 165) // 9-class Greens #4

}