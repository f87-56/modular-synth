package SynthLogic

import scala.util.{Failure, Success, Try}

// Suggestion: Value classes may be a good fit for creating "signal types"

/**
 * @tparam T The type of signal this component outputs
 */
trait SynthComponent[+T<:SignalType] {
  val parameters:Seq[Parameter[SignalType]]

  /**
   * Within Synthcomponent, access a parameter with its proper type.
   * This allows us to circumvent casting.
   * @param name  The name of the parameter
   * @tparam U  The signal type of the parameter
   * @return
   */
  def param[U<:SignalType](name:String):Try[Parameter[U]] =
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
    param[U](name).map(_.value)

  def output:T
}

// TODO : Provide operators for wiring these together
object SynthComponent:

end SynthComponent

