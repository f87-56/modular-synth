package SynthLogic

import scala.util.{Failure, Success, Try}

// Suggestion: Value classes may be a good fit for creating "signal types"

/**
 * I am, at the present, dissatisfied with the implementation of this class.
 * Too much relies on the programmer's knowlege of the inner workings.
 * @tparam T
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
  def param[U<:SignalType](name:String):Try[U] =
    val rightType:Seq[Parameter[U]] = parameters.collect{
      case a:Parameter[U] => a
    }
    val valueMap:Map[String, U] = rightType.map(a => a.name).zip(rightType.map(_.value))
      .toMap
    valueMap.get(name) match
      case Some(n) => Success(n)
      case None => Failure(java.util.NoSuchElementException())

  def output:T
}
object SynthComponent:

end SynthComponent

