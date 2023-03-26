package SynthLogic

import SynthLogic.ModularSynthesizer.uproot

import javax.sound.midi.{MidiMessage, ShortMessage}
import scala.collection.mutable

/**
 * Does not support polyphony yet
 */

/**
 * Represents our modular synthesizer.
 *
 * The only way a ModularSynthesizer object can be modified is by changing the
 * wiring of the components.
 *
 * @param pComponents The components that the synth contains. What is listed here
 *                   doesn't matter all that much, all the connected components will
 *                   be automatically found.
 * @param finalGather The component that connects to the output. May be missing.
 */
class ModularSynthesizer(val sampleRate:Int) {
  /**
   * Gahter all the components that are actually part of the synth tree
   * (plus the extra ones specified in comonents that may not be part of it.
   */
  private val components_ = mutable.Set[SynthComponent[_]]()
    //mutable.Set(pComponents.flatMap(uproot(_, Set())).toSet ++ Set(finalGather).flatten)
  final def components: Vector [SynthComponent[_]] = components_.toVector
  
  final def addComponent(cmp:SynthComponent[_]):Unit =
    components_ += cmp
  final def removeComponent(cmp:SynthComponent[_]):Unit=
    cmp.xAll()
    components_ -= cmp

  final val outputComponent = ComponentLibrary.passthrough(this)

  val deltaTime: Double = 1.0/sampleRate
  
  // Here just to manage calls, no practical use yet.
  //val voices:Array[Option[RuntimeContext]] = Array.fill[Option[RuntimeContext]](16)(None)

  // Our voices give the synth the values for its input variables
  private var voice_ = Voice.init(sampleRate)
  // this should be fine I think
  def voice: Voice = voice_

  // Update the runtime contexts and give the output note
  final def output(midiMessage: Option[MidiMessage]):Double =
    val out = outputComponent.output
    voice_ = voice_.stepTime(midiMessage)
    out.getOrElse(0)
}

/**
 * The companion object contains factory methods
 */
object ModularSynthesizer:

  /**
   * Chase down all components that are connected to this one.
   * @param component The component in question
   * @return A set of comoponents that trace themeselves to component.
   */
  def uproot(component:SynthComponent[_],
             exclude:Set[SynthComponent[_]]):Set[SynthComponent[_]]=
    val immediatePrev:Set[SynthComponent[_]] = component.parameters.flatMap(_.getInput).toSet.diff(exclude)
    (immediatePrev ++ immediatePrev.flatMap(uproot(_, immediatePrev ++ exclude))) + component

  def default: ModularSynthesizer =
    val aa = ModularSynthesizer(44100)
    val a = ComponentLibrary.Oscillator(aa)
    val b = ComponentLibrary.Amplifier(aa)
    val c = ComponentLibrary.Envelope(aa)
    aa.outputComponent.input <== b
    b.gain <== c
    b.input <== a
    aa

end ModularSynthesizer

/**
 *
 * Relays information of the local execution environment
 * @param sample The number of the sample (since the start of the runtime)
 * @param keyFrequency the base frequency of the note to be generated
 * @param keyDown Is the key that controls this context pressed?
 * @param primary Is this the primary voice? One such voice will NOT be terminated.
 */
case class Voice(sample:Int, keyFrequency:Double, message:Option[MidiMessage], sampleRate:Int, primary:Boolean):
  def stepTime(midiMessage: Option[MidiMessage]) = Voice(sample + 1, keyFrequency, midiMessage, sampleRate, primary)
object Voice:
  def initPrimary(sampleRate:Int) = Voice(0, 0.0, None, sampleRate, true)
  def init(sampleRate:Int) = Voice(1, 0.0, None, sampleRate, false)

