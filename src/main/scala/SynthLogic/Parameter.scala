package SynthLogic

sealed trait SignalType:
  type T
  val value:T

class BoolSignal(val value:Boolean = false) extends SignalType:
  type T = Boolean

class DoubleSignal(val value:Double = 0.0) extends SignalType:
  type T = Double

class IntSignal(val value:Int = 0) extends SignalType:
  type T = Int

class DoubleVectorSignal(val value:Vector[Double] = Vector()) extends SignalType:
  type T = Vector[Double]

class DoubleMapSignal(override val value: Map[Double, Double] = Map()) extends SignalType:
  type T = Map[Double, Double]

/**
 *
 * @param name The visible name of this parameter
 * @param description A description for the parameter
 * @param takesInput Can this parameter take input from another SynthComponent?
 * @param defaultValue Default value to be given if no input exists
 * @param input The (optional) SynthComponent that feeds data to this parameter
 * @tparam T  The signal type of this parameter
 */
case class Parameter[+T<:SignalType](name:String, description:String, takesInput:Boolean = true, defaultValue:T,
                                    input:Option[SynthComponent[T]] = None):

  def value:T = input.map(_.output).getOrElse(defaultValue)
end Parameter
object Parameter:
end Parameter

/**
 * For parameters where discrete choices are presented
 */
trait EnumerableParam(choices:String*):
  this: Parameter[IntSignal] =>
  def enumValue:String = choices.lift(this.value.value).getOrElse("")