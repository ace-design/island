package eu.ace_design.island.viewer


import eu.ace_design.island.map._
import eu.ace_design.island.util._
import java.io.File


/**
 * A viewer is an element used to represent a Mesh, processing it into a viewable file
 */
trait Viewer extends Logger {

  val silo = LogSilos.VIEWER

  /**
   * A viewer produces files with a given extension, usually compatible with external viewers.
   * @return the extension to be used
   */
  def extension: String

  /**
   * The MIME type used by this viewer (for the processed output file)
   * @return  a valid MIME type
   */
  def mimeType: String
  /**
   * A viewer is a function, transforming an IslandMap into a viewable file.
   * Thus, we use Scala syntactic sugar to support it
   *
   * @param m the map one wants to visualize
   * @return a File containing the associated representation
   */
  def apply(m: IslandMap): File

  /**
   * Protected method used to generate a temporary file as an output
   * @return
   */
  protected def initOutput: File = File.createTempFile("island-viewer-", "."+extension)

}

