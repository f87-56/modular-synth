package SynthFileIO

import SynthGUI.SynthCanvas
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.parser.decode
import SynthLogic.{ModularSynthesizer, SynthComponent}

import scala.util.{Success, Try}

object SynthSerializer:

  def loadCanvas(fileName:String):Try[SynthCanvas] =
    val fileText = FileManager.readFile(fileName).map(_.mkString("\n"))
    val canvas = fileText.flatMap(decode[SynthCanvas](_).toTry)
    canvas
  def saveCanvas(synthCanvas: SynthCanvas, fileName:String) =
    FileManager.writeFile(fileName, synthCanvas.asJson.toString)
  def decodeComponents(synthJSON:String) =
    decode[List[String]](synthJSON).toTry

end SynthSerializer