package SynthLogic

import SynthLogic.ModularSynthesizer.uproot
import SynthSoundIO.SynthRuntime
import io.circe.{Decoder, Encoder, HCursor, Json, syntax}
import io.circe.syntax.*
import SynthComponent.compDataDecoder

import java.util.zip.DataFormatException
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
  private val components_ = mutable.Buffer[SynthComponent[_]]()
    //mutable.Set(pComponents.flatMap(uproot(_, Set())).toSet ++ Set(finalGather).flatten)
  final def components: Vector [SynthComponent[_]] = components_.toVector
  
  final def addComponent(cmp:SynthComponent[_]):Unit =
    if !components_.contains(cmp) then
      components_ += cmp
  final def removeComponent(cmp:SynthComponent[_]):Unit=
    // Don't remove the output component.
    if !(outputComponent == cmp) then
      cmp.xAll()
      components_ -= cmp

  final val outputComponent = ComponentLibrary.PassThrough(this, None)

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
    val a = ComponentLibrary.Oscillator(aa, Some("Oscillator"))
    val b = ComponentLibrary.Amplifier(aa, Some("Amplifier"))
    val c = ComponentLibrary.Envelope(aa, Some("Envelope"))

    //-------------TEST
    //val testStr = ComponentLibrary.TestComp(aa)
    //-------------TEST

    aa.outputComponent.input <== b
    b.gain <== c
    //--------------TEST
    //b.gain <== testStr
    //--------------TEST
    b.input <== a
    aa

  given Encoder[ModularSynthesizer] = (a:ModularSynthesizer) => Json.obj(
    ("Components", a.components.asJson),
    ("OutputComponentIndex", Json.fromInt(a.components.indexOf(a.outputComponent)))
  )

  given Decoder[ModularSynthesizer] = (c:HCursor) => for
    // List(Id, param list)
    comps <- c.downField("Components").as[List[(String,List[(Int,String)])]]
    outIndex <- c.downField("OutputComponentIndex").as[Int]
  yield
    val synth = ModularSynthesizer(SynthRuntime.BIT_RATE)
    val components = comps.zipWithIndex.map { a =>
      // Get the output component that automatically gets construced
      if (a._2 != outIndex) then
        ComponentLibrary.createComponent(a._1._1, synth)
      else
        Some(synth.outputComponent)
    }
    // Some component could not be read. throw.
    if(components.exists(_.isEmpty)) then throw DataFormatException()

    val paramInfo = comps.map(_._2)
    val realParams = components.map(_.map(_.parameters))
    // (Option(Vector(param)), Vector(paaramInfo))
    (realParams zip paramInfo).foreach(
            // (param), paramInfo)
      a =>
        (a._1.getOrElse(Vector()) zip a._2).foreach(
        // Set them up
        b =>
          // We only allowed certain custom types
          // Not a very good design choice, we have to specify allowed types in several places
          b._1.defaultValue =
            b._1.defaultValue match
              case i:Int => b._2._2.toIntOption.getOrElse(b._1.defaultValue)
              case d:Double =>  b._2._2.toDoubleOption.getOrElse(b._1.defaultValue)
              case bo:Boolean => b._2._2.toBooleanOption.getOrElse(b._1.defaultValue)
              case s:String => b._2._2
              case _ => ()
          synth.components.lift(b._2._1).foreach(b._1 <== _)
      )
    )
    synth

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

