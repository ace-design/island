package eu.ace_design.island.viewer

import eu.ace_design.island.geom.Point
import eu.ace_design.island.map.{HasForHeight, IslandMap}
import eu.ace_design.island.map.processes.{ElevationFunctions, AssignElevation}
import org.specs2.mutable._
import org.specs2.matcher.{XmlMatchers, FileMatchers}


class ViewerTest extends SpecificationWithJUnit with FileMatchers with XmlMatchers {

  // Mandatory to use the specific XML factory and disable document validation (too slow when activated)
  System.setProperty("javax.xml.parsers.SAXParserFactory", classOf[MyXMLParserFactory].getName)

  "ViewerTest Specifications".title

  val mesh = eu.ace_design.island.geom.MeshBuilderTestDataSet.mesh
  val map = AssignElevation(ElevationFunctions.identity)(IslandMap(mesh))
  val tika =  new org.apache.tika.Tika()

  "The SVG viewer" should {
    val toSVG = new SVGViewer()
    val file = toSVG(map)
    val xml = scala.xml.XML.loadFile(file)

    "use 'svg' as extension" in { toSVG.extension must_== "svg" }
    "process a map into a file" in {
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
  }

  "The PDF viewer" should {
    val toPDF = new PDFViewer()
    val file = toPDF(map)
    "use 'pdf' as extension" in { toPDF.extension must_== "pdf" }
    "process a map into a file" in {
      file must beAFile
      file must beReadable
    }
    "create a file recognized as a PDF" in {
      tika.detect(file.getAbsolutePath) must_== toPDF.mimeType
    }
  }

  "The OBJ viewer" should {
    val toObj = new OBJViewer()
    val file = toObj(map)
    val contents = scala.io.Source.fromFile(file).getLines().toSeq
    "uses obj as extension" in { toObj.extension must_== "obj" }
    "process a map into a file" in {
      file must beAFile
      file must beReadable
    }
    "create a file recognized as a plain text" in {
      tika.detect(file.getAbsolutePath) must_== toObj.mimeType
    }
    "contain each stored vertex" in {
      def isValid(idx: Int, data: Seq[String]) = {  // v $x $y $z
        data must haveSize(4)
        data(0) must_== "v"
        map.vertices must contain(Point(data(1).toDouble, data(2).toDouble))
        val z = try { map.vertexProps.getValue(idx, HasForHeight()) } catch { case e: IllegalArgumentException => 0.0 }
        data(3).toDouble must_== z
      }
      val vertices = contents filter { s => s.startsWith("v") }
      (0 until vertices.size) foreach { idx => isValid(idx, vertices(idx).split(" ")) }
      vertices must haveSize(map.vertices.size)
    }
    "contain the same number of faces" in {
      // WARNING this test is an over simplification of what should be done (check the contents of each face)
      val faces = contents filter { s => s.startsWith("f") }
      faces must haveSize(map.faces.size)
    }
  }


  "the JSON viewer" should {
    val toJson = new JsonViewer()
    val file = toJson(map)
    "use json as extension" in { toJson.extension must_== "json" }
    "process a map into a file" in {
      file must beAFile
      file must beReadable
    }
    "create a file recognized as a json" in {
      tika.detect(file.getAbsolutePath) must_== toJson.mimeType
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
