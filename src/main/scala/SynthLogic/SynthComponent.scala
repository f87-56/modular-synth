package SynthLogic

import scala.util.{Failure, Success, Try}

// Suggestion: Value classes may be a good fit for creating "signal types"

/**
 * TODO: Fit this with the ability to store the last computed value, and checking for dirty.
 * @tparam T The type of signal this component outputs
 */
trait SynthComponent[+T] {

  final private val _parameters:scala.collection.mutable.Buffer[Parameter[_]]
    = scala.collection.mutable.Buffer()
  final def parameters = _parameters
  def output(runtimeContext: RuntimeContext):T

  // The parameters (of other components) this component outputs to
  // Here, we don't care about the types, since that is handled by
  // Parameter
  final private val connections:scala.collection.mutable.Set[Parameter[_]] =
    scala.collection.mutable.Set[Parameter[_]]()
  final def addParameter(p:Parameter[_]) = _parameters += p

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
    // remove all
    this.connections.foreach(_.x())
    this.connections.clear()
    this.parameters.foreach(_.x())


  // Disconnects all from the output side.
  // EFFECTFUL FUNCTION
  final def xOutputs(): Unit =
  // remove all
    this.connections.foreach(_.x())
    this.connections.clear()
}

object SynthComponent:

end SynthComponent

