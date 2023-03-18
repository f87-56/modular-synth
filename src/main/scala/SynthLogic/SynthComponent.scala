package SynthLogic

import scala.util.{Failure, Success, Try}

// Suggestion: Value classes may be a good fit for creating "signal types"

/**
 * @tparam T The type of signal this component outputs
 */
trait SynthComponent[+T<:SignalType] {
  val parameters:Seq[Parameter[SignalType]]
  def output:T

  // The parameters (of other components) this component outputs to
  // Here, we don't care about the types, since that is handled by
  // Parameter
  // OBSERVE: IT IS MUTABLE
  private val connections:scala.collection.mutable.Set[Parameter[SignalType]] =
    scala.collection.mutable.Set[Parameter[SignalType]]()

  /**
   * Within Synthcomponent, access a parameter with its proper type.
   * This allows us to circumvent casting.
   * @param name  The name of the parameter
   * @tparam U  The signal type of the parameter
   * @return
   */
  private def parameter[U<:SignalType](name:String):Try[Parameter[U]] =
    val rightType:Seq[Parameter[U]] = parameters.collect{
      case a:Parameter[U] => a
    }
    val valueMap:Map[String, Parameter[U]] = rightType.map(a => a.name).zip(rightType)
      .toMap
    valueMap.get(name) match
      case Some(n) => Success(n)
      case None => Failure(java.util.NoSuchElementException())

  /**
   * Tries to get the value of a parameter with the specified signature
    * @param name The name of the parameter
   * @tparam U  The signal type of the parameter
   * @return
   */
  def paramValue[U<:SignalType](name:String):Try[U] =
    parameter[U](name).map(_.value)

  // NOT TO BE USED OUTSIDE THE CLASS PARAMETER
  // I haven't figured out how I could prevent this.
  final def addConnection(parameter: Parameter[SignalType]):Unit =
    connections += parameter

  // NOT TO BE USED OUTSIDE THE CLASS PARAMETER
  final def disconnect(parameter: Parameter[SignalType]):Unit =
    this.connections -= parameter

  // Disconnects everything from this SynthComponent.
  final def xAll(): Unit =
    // remove all
    this.connections.foreach(_.x())
    this.connections.clear()
    this.parameters.foreach(_.x())


  // Disconnects all from the output side.
  final def xOutputs(): Unit =
  // remove all
    this.connections.foreach(_.x())
    this.connections.clear()
}

object SynthComponent:

end SynthComponent

