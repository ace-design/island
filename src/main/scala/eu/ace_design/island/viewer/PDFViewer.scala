package eu.ace_design.island.viewer

import java.io.File
import eu.ace_design.island.map.IslandMap
import eu.ace_design.island.viewer.svg.SVGBiomeViewer

class PDFViewer extends Viewer {
  import org.apache.batik.apps.rasterizer.{DestinationType, SVGConverter}

  override val extension: String = "pdf"
  override val mimeType: String = "application/pdf"

  override def apply(m: IslandMap): File = {
    val result = initOutput

    // We first create an SVG file with the SVG viewer:
    val svgFile = (new SVGBiomeViewer())(m)

    // We leverage the Batik rasterizer
    val converter = new SVGConverter()
    converter.setDestinationType(DestinationType.PDF)
    converter.setSources(Array(svgFile.getAbsolutePath))
    converter.setDst(result)

    // Running the converter and eventually returning the result
    // Batik has a f*cking System.out.println() instruction in its source => ugly patch
    info("Converting SVG to PDF")
    System.setOut(new java.io.PrintStream(new java.io.ByteArrayOutputStream()))
    converter.execute()
    System.setOut(new java.io.PrintStream(new java.io.FileOutputStream(java.io.FileDescriptor.out)))
    result
  }

}