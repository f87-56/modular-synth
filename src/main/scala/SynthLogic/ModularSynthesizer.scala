package SynthLogic

/**
 * Does not support polyphony yet
 */
class ModularSynthesizer(finalGather:SynthComponent[DoubleSignal]) {
  // Here just to manage calls, no practical use yet.
  val voices:Array[Int] = ???
  val outputComponent: ComponentLibrary.PassThrough = ???
  def output:Double = outputComponent.output.value
}

/**
 * The companion object contains factory methods
 */
object ModularSynthesizer:
  def default: ModularSynthesizer =
    ModularSynthesizer(ComponentLibrary.Oscillator())

end ModularSynthesizer
