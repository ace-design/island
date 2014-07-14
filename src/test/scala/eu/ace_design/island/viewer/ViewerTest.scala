package eu.ace_design.island.viewer

import org.specs2.mutable._
import org.specs2.matcher.FileMatchers
import java.nio.file.{Path, Files, Paths}


class ViewerTest extends SpecificationWithJUnit with FileMatchers {

  "ViewerTest Specifications".title

  "the SVG viewer" should {
    val toSVG = new SVGViewer()
    "use 'svg' as extension" in { toSVG.extension must_== "svg" }
    "process a mesh into an SVG file" in {
      val file = toSVG(eu.ace_design.island.geom.MeshBuilderTestDataSet.mesh)
      file must beAFile
      file must beReadable
      Files.probeContentType(Paths.get(file.getPath)) must_== toSVG.mimeType
    }
  }
}

/**
 * Leverage the Tika library for MIME type detection
 * This file detector is registered in src/test/resources/META-INF/services/java.nio.file.spi.FileTypeDetector
 * http://odoepner.wordpress.com/2013/07/29/transparently-improve-java-7-mime-type-recognition-with-apache-tika/
 */
class FileDetector extends java.nio.file.spi.FileTypeDetector {
  override def probeContentType(path: Path): String = {
    new org.apache.tika.Tika().detect(path.toFile)
  }
}
