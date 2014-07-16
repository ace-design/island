package eu.ace_design.island.viewer

import org.specs2.mutable._
import org.specs2.matcher.{XmlMatchers, FileMatchers}
import java.nio.file.{Path, Files, Paths}


class ViewerTest extends SpecificationWithJUnit with FileMatchers with XmlMatchers {

  // Mandatory to use the specific XML factory and disable document validation (too slow when activated)
  System.setProperty("javax.xml.parsers.SAXParserFactory", classOf[MyXMLParserFactory].getName)

  "ViewerTest Specifications".title

  val mesh = eu.ace_design.island.geom.MeshBuilderTestDataSet.mesh
  val tika =  new org.apache.tika.Tika()

  "The SVG viewer" should {
    val toSVG = new SVGViewer()
    val file = toSVG(mesh)
    val xml = scala.xml.XML.loadFile(file)

    "use 'svg' as extension" in { toSVG.extension must_== "svg" }
    "process a mesh into a file" in {
      file must beAFile
      file must beReadable
    }
    "create a file recognized as SVG" in {
      tika.detect(file.getAbsolutePath) must_== toSVG.mimeType
    }
    "respect the dimension for the mesh" in {
      mesh.size match {
        case Some(s) => {
          (xml \ "@width").toString aka "the width"   must_== s"$s"
          (xml \ "@height").toString aka "the height" must_== s"$s"
        }
        case None => true must beTrue
      }
    }
    "Contains as many 'path' as given faces" in {
      xml \\ "path" must haveSize(mesh.faces.size)
    }
  }

  "The PDF viewer" should {
    val toPDF = new PDFViewer()
    val file = toPDF(mesh)
    "use 'pdf' as extension" in { toPDF.extension must_== "pdf" }
    "process a mesh into a file" in {
      file must beAFile
      file must beReadable
    }
    "create a file recognized as a PDF" in {
      tika.detect(file.getAbsolutePath) must_== toPDF.mimeType
    }
  }
}

/**
 * Home made xml parser factory with restriction to not validate the loaded files (the response time is too slow
 * when validation is enabled)
 * ref: http://stackoverflow.com/questions/1096285/is-scala-java-not-respecting-w3-excess-dtd-traffic-specs
 */
class MyXMLParserFactory extends com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl {
  super.setFeature("http://xml.org/sax/features/validation", false)
  super.setFeature("http://apache.org/xml/features/disallow-doctype-decl", false)
  super.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
  super.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
}
