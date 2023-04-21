package SynthFileIO

import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.parser.decode
import SynthLogic.{ModularSynthesizer, SynthComponent}

import scala.util.Try

class SynthSerializer:
  def decodeComponents(synthJSON:String) =
    decode[List[String]](synthJSON).toTry

end SynthSerializer