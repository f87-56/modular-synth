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
    val fileText: Try[String] = FileManager.readFile(file).map(_.mkString("\n"))
    val canvas = fileText.flatMap(decode[SynthCanvas](_).toTry)
    OutputLog.log("Loading synth from: " + file)
    canvas

  def loadCanvas(jsonText:String):Try[SynthCanvas] =
    println("AHOY")
    val canvas = decode[SynthCanvas](jsonText).toTry
    OutputLog.log("Loading synth from string literal")
    canvas

  // Side effects here
  // TODO: Save with file extension
  def saveCanvas(synthCanvas: SynthCanvas, file:File): Unit =
    FileManager.writeFile(file, synthCanvas.asJson.toString)
    OutputLog.log("Saving synth to: " + file)
  def decodeComponents(synthJSON:String): Try[List[String]] =
    decode[List[String]](synthJSON).toTry

  val EmptyWorkspace: String =
    """
      |{
      |  "Synth" : {
      |    "Components" : [
      |      {
      |        "IdentifyingName" : "",
      |        "Parameters" : [
      |          {
      |            "Index" : -1,
      |            "DefaultValue" : "0.5"
      |          }
      |        ]
      |      }
      |    ],
      |    "OutputComponentIndex" : 0
      |  },
      |  "VisualComponents" : [
      |    {
      |      "SynthComponentIndex" : 0,
      |      "Position" : [
      |        2324.632080078125,
      |        1168.335205078125
      |      ]
      |    }
      |  ]
      |}
      |
      |""".stripMargin

end SynthSerializer