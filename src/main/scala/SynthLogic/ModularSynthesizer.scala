package SynthLogic

import SynthLogic.ModularSynthesizer.uproot

import javax.sound.midi.{MidiMessage, ShortMessage}

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
                         val finalGather:Option[SynthComponent[DoubleSignal]], val sampleRate:Int) {
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

  // Just for initial construction
  private var time = 0


  // Here just to manage calls, no practical use yet.
  //val voices:Array[Option[RuntimeContext]] = Array.fill[Option[RuntimeContext]](16)(None)

  // Our voices give the synth the values for its input variables
  private var voice: RuntimeContext = RuntimeContext.init(sampleRate)
  // Update the runtime contexts and give the output note
  def output(midiMessage: Option[ShortMessage]):Double =
    val out = outputComponent.output(voice).value
    voice = voice.stepTime(midiMessage)
    out
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
    ModularSynthesizer((previousSynth.components + newComponent).toVector, previousSynth.finalGather, previousSynth.sampleRate)

  def removeComponent(previousSynth: ModularSynthesizer, toRemove: SynthComponent[SignalType]) =
    toRemove.xAll()
    if(previousSynth.finalGather.contains(toRemove)) then
      ModularSynthesizer((previousSynth.components - toRemove).toVector, None, previousSynth.sampleRate)
    else
      ModularSynthesizer((previousSynth.components - toRemove).toVector, previousSynth.finalGather, previousSynth.sampleRate)

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
    ModularSynthesizer(Vector(), Some(ComponentLibrary.Oscillator()), 44100)

end ModularSynthesizer

/**
 *
 * Relays information of the local execution environment
 * @param sample The number of the sample (since the start of the runtime)
 * @param keyFrequency the base frequency of the note to be generated
 * @param keyDown Is the key that controls this context pressed?
 * @param primary Is this the primary voice? One such voice will NOT be terminated.
 */
case class RuntimeContext(sample:Int, keyFrequency:Double, message:Option[ShortMessage], sampleRate:Int, primary:Boolean):
  def stepTime(midiMessage: Option[ShortMessage]) = RuntimeContext(sample + 1, keyFrequency, midiMessage, sampleRate, primary)
object RuntimeContext:
  def initPrimary(sampleRate:Int) = RuntimeContext(0, 0.0, None, sampleRate, true)
  def init(sampleRate:Int) = RuntimeContext(1, 0.0, None, sampleRate, false)

