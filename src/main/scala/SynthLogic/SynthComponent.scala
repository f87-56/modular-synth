package SynthLogic

import java.util.OptionalInt
import scala.util.{Failure, Success, Try}

// Suggestion: Value classes may be a good fit for creating "signal types"

/**
 * @tparam T The type of signal this component outputs
 */
trait SynthComponent[+T](val host:ModularSynthesizer) {
  
  host.addComponent(this)

  private val _parameters:scala.collection.mutable.Set[Parameter[_]]
    = scala.collection.mutable.Set()
  final def parameters: Vector[Parameter[_]] = _parameters.toVector
  def compute:T

  // A bit of a hack to circumvent type erasure
  lazy val initialValue: T = compute

  // What is the number of the previous computed sample?
  protected var prevSample = -1

  private[this] var prevValue:Option[T] = None

  // We may want helper functions for easily getting info from the host's voice.

  // Recalculate only if time has advanced.
  def output: Option[T] =
    if host.voice.sample == prevSample then
      prevValue
    else
      Some(compute)

  // The parameters (of other components) this component outputs to
  // Here, we don't care about the types, since that is handled by
  // Parameter
  final private val connections:scala.collection.mutable.Set[Parameter[_]] =
    scala.collection.mutable.Set[Parameter[_]]()

  // NOT TO BE USED OUTSIDE THE CLASS PARAMETER
  final def addParameter(p:Parameter[_]):Unit = _parameters += p

  // NOT TO BE USED OUTSIDE THE CLASS PARAMETER
  // I haven't figured out how I could prevent this.
  final def addConnection(parameter: Parameter[_]):Unit =
    connections += parameter

  // NOT TO BE USED OUTSIDE THE CLASS PARAMETER
  final def disconnect(parameter: Parameter[_]):Unit =
    this.connections -= parameter

  // Disconnects everything from this SynthComponent.
  // EFFECTFUL FUNCTION
  final def xAll(): Unit =
    // cut all connections, forward and back
    this.connections.foreach(_.x())
    this.connections.clear()
    this.parameters.foreach(_.x())


  // Cut all forward connections
  // EFFECTFUL FUNCTION
  final def xOutputs(): Unit =
  // remove all
    this.connections.foreach(_.x())
    this.connections.clear()
}

object SynthComponent:

end SynthComponent

