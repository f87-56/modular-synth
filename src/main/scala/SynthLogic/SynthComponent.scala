package SynthLogic

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax.*
import Parameter.{given_Decoder_Int_String, *}

import java.util.OptionalInt
import scala.util.{Failure, Success, Try}

// Suggestion: Value classes may be a good fit for creating "signal types"

/**
 *
 * @tparam T
 */
trait SynthComponent[+T](val host:ModularSynthesizer, val serializationTag:Option[String] = None):
  
  host.addComponent(this)

  private val _parameters:scala.collection.mutable.Set[Parameter[_]]
    = scala.collection.mutable.Set()
  final def parameters: Vector[Parameter[_]] = _parameters.toVector
  def compute:T
  
  // A bit of a hack to circumvent type erasure (DOES NOT WORK)
  lazy val initialValue: T = compute

  // What is the number of the previous computed sample?
  protected var prevSample = -1

  private[this] var prevValue:Option[T] = None

  protected val tickTime = 1

  // We may want helper functions for easily getting info from the host's voice.

  // Recalculate only if time has advanced and we want to tick.
  def output: Option[T] =
    if (host.voice.sample == prevSample || host.voice.sample % tickTime != 0) then
      prevValue
    else
      Some(compute)

  // The parameters (of other components) this component outputs to
  // Here, we don't care about the types, since that is handled by
  // Parameter
  final private val _connections:scala.collection.mutable.Set[Parameter[_]] =
    scala.collection.mutable.Set[Parameter[_]]()

  // Return the current connections
  def connections: Seq[Parameter[_]] = _connections.toVector
  
  // NOT TO BE USED OUTSIDE THE CLASS PARAMETER
  final def addParameter(p:Parameter[_]):Unit = _parameters += p

  // NOT TO BE USED OUTSIDE THE CLASS PARAMETER
  // I haven't figured out how I could prevent this.
  final def addConnection(parameter: Parameter[_]):Unit =
    _connections += parameter

  // NOT TO BE USED OUTSIDE THE CLASS PARAMETER
  final def disconnect(parameter: Parameter[_]):Unit =
    this._connections -= parameter

  // Disconnects everything from this SynthComponent.
  // EFFECTFUL FUNCTION
  final def xAll(): Unit =
    // cut all connections, forward and back
    this._connections.foreach(_.x())
    this._connections.clear()
    this.parameters.foreach(_.x())


  // Cut all forward connections
  // EFFECTFUL FUNCTION
  final def xOutputs(): Unit =
  // remove all
    this._connections.foreach(_.x())
    this._connections.clear()

end SynthComponent
object SynthComponent:


  given Encoder[SynthComponent[_]] = (a: SynthComponent[_]) => Json.obj(
    // The identifier in the ComponentLibrary
    ("IdentifyingName", Json.fromString(a.serializationTag.getOrElse(""))),
    ("Parameters", a.parameters.asJson)
  )
  // The decoder
  given compDataDecoder:Decoder[(String, List[(Int, String)])] = (c: HCursor) => for
    identifier <- c.downField("IdentifyingName").as[String]
    params <- c.downField("Parameters").as[List[(Int, String)]]
  yield
    (identifier, params)

end SynthComponent

