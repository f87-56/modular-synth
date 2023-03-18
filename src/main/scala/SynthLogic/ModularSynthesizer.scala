package SynthLogic

import SynthLogic.ModularSynthesizer.uproot

/**
 * Does not support polyphony yet
 */

/**
 * Represents our modular synthesizer.
 *
 * The only way a ModularSynthesizer object can be modified is by changing the
 * wiring of the components.
 *
 * @param components The components that the synth contains. What is listed here
 *                   doesn't matter all that much, all the connected components will
 *                   be automatically found.
 * @param finalGather The component that connects to the output. May be missing.
 */
class ModularSynthesizer(pComponents:Vector[SynthComponent[SignalType]],
                         val finalGather:Option[SynthComponent[DoubleSignal]]) {
  /**
   * Gahter all the components that are actually part of the synth tree
   * (plus the extra ones specified in comonents that may not be part of it.
   */
  private val components_ : Set[SynthComponent[SignalType]] =
    pComponents.flatMap(uproot(_, Set())).toSet ++ Set(finalGather).flatten
  def components: Set[SynthComponent[SignalType]] = components_

  val outputComponent = ComponentLibrary.passthrough
  // Connect the finalGather to the output.
  finalGather.foreach(
    outputComponent.parameters.head <== _
  )

  // Here just to manage calls, no practical use yet.
  val voices:Array[Int] = Array.ofDim[Int](16)
  def output:Double = outputComponent.output.value
}

/**
 * The companion object contains factory methods
 */
object ModularSynthesizer:

  /**
   * Adding a component to a synthesizer.
   * @param previousSynth
   * @param newComponent
   */
  def addComponent(previousSynth: ModularSynthesizer, newComponent:SynthComponent[SignalType]) =
    ModularSynthesizer((previousSynth.components + newComponent).toVector, previousSynth.finalGather)

  def removeComponent(previousSynth: ModularSynthesizer, toRemove: SynthComponent[SignalType]) =
    toRemove.xAll()
    if(previousSynth.finalGather.contains(toRemove)) then
      ModularSynthesizer((previousSynth.components - toRemove).toVector, None)
    else
      ModularSynthesizer((previousSynth.components - toRemove).toVector, previousSynth.finalGather)

  /**
   * Chase down all components that are connected to thes one.
   * @param component The component in question
   * @return A set of comoponents that trace themeselves to component.
   */
  def uproot(component:SynthComponent[SignalType],
             exclude:Set[SynthComponent[SignalType]]):Set[SynthComponent[SignalType]]=
    val immediatePrev:Set[SynthComponent[SignalType]] = component.parameters.flatMap(_.getInput).toSet.diff(exclude)
    (immediatePrev ++ immediatePrev.flatMap(uproot(_, immediatePrev ++ exclude))) + component

  def default: ModularSynthesizer =
    ModularSynthesizer(Vector(), Some(ComponentLibrary.Oscillator()))

end ModularSynthesizer
