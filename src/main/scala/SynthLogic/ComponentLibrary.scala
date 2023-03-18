package SynthLogic

import SynthUtilities.*

object ComponentLibrary {

  /**
   * An identity operation. A component that passes through its input unchanged.
   */
  class PassThrough extends SynthComponent[DoubleSignal]():
    val parameters: Seq[Parameter[DoubleSignal]] =
      Vector(Parameter[DoubleSignal]("input","", true, DoubleSignal(0.5), this))
    // I am fairly satisfied as to how this looks.
    override def output: DoubleSignal = paramValue[DoubleSignal]("input").getOrElse(DoubleSignal(0.0))

  /**
   * A simple oscillator
   */
  class Oscillator extends SynthComponent[DoubleSignal]():
    override val parameters: Seq[Parameter[SignalType]] =
      Vector(
        new Parameter[IntSignal]("type", "", false,  IntSignal(0),this) with EnumerableParam("sine", "square", "sawtooth", "noise")
      )
    val time:Double = .0
    override def output: DoubleSignal = DoubleSignal(MathUtilities.parametricSin(1,2*math.Pi*440,0,0,time))

  def passthrough: PassThrough = PassThrough()

}
