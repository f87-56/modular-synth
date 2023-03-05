package SynthLogic

import sun.misc.Signal

import java.util.function.DoubleUnaryOperator

// the different types of data our parameters can take. Each hold different kinds of data.
sealed trait Signal:
  type T
  def value:T
end Signal


// The Value traits represent a discrete value
// The Signal traits represent a collection of values
trait BoolValue extends Signal:
  type T = Boolean
/**
 * The EnumSignal can only take discrete values
 */
trait EnumValue extends Signal:
  type T = Int
trait ByteValue extends Signal:
  type T = Byte

trait FloatValue extends Signal:
  type T = Double

trait FloatSignal extends Signal:
  type T = Vector[Double]

/**
 * For frequency domain signal processing
 */
trait FreqDomainSignal extends Signal:
  type T = Map[Double, Double]

/**
 * All synth parameters are treated as signals.
 * @param name
 * @param descrition
 * @param defaultValue The default value of a parameter is a signal (For example, a constant-valued signal!)
 * @tparam U The type of signal this parameter represents.
 */
class Parameter[U<:Signal](name:String, descrition:String, takesInput:Boolean = true, defaultValue:U,
                          input:Option[SynthComponent[U]] = None):
  def value:U = input.map(_.output).getOrElse(defaultValue)

