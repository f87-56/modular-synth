import io.circe.syntax.*
import io.circe.parser.decode
import SynthLogic.Parameter.{given_Decoder_Int_String, *}
import SynthLogic.SynthComponent
import io.circe.{Decoder, HCursor}
import SynthLogic.SynthComponent.compDataDecoder
@main def jsonTests():Unit =

  val testStr =
    """
      |
      | {
      |        "IdentifyingName" : "",
      |        "Parameters" : [
      |          {
      |            "Index" : 3,
      |            "DefaultValue" : "1.0"
      |          },
      |          {
      |            "Index" : 0,
      |            "DefaultValue" : "1.0"
      |          }
      |        ]
      |      }
      |""".stripMargin

  val a = decode[(String,List[(Int, String)])](testStr)
  println(a)

