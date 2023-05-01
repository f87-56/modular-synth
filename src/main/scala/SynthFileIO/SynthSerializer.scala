package SynthFileIO

import SynthGUI.{OutputLog, SynthCanvas}
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.parser.decode
import SynthLogic.{ModularSynthesizer, SynthComponent}

import java.io.File
import scala.util.{Success, Try}

object SynthSerializer:

  def loadCanvas(file:File):Try[SynthCanvas] =
    val fileText = FileManager.readFile(file).map(_.mkString("\n"))
    val canvas = fileText.flatMap(decode[SynthCanvas](_).toTry)
    OutputLog.log("Loading synth from: " + file)
    canvas
    
  // Side effects here
  def saveCanvas(synthCanvas: SynthCanvas, file:File): Unit =
    FileManager.writeFile(file, synthCanvas.asJson.toString)
    OutputLog.log("Saving synth to: " + file)
  def decodeComponents(synthJSON:String) =
    decode[List[String]](synthJSON).toTry

end SynthSerializer