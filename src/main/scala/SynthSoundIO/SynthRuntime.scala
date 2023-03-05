package SynthSoundIO
import SynthLogic.ModularSynthesizer

import javax.sound.midi
import javax.sound.midi.{MidiMessage, Receiver, Transmitter}

/**
 * The "Executuion ground" for modular synthesizers. Handles passing information between the sound system and modular synthesizers.
 */
abstract class SynthRuntime extends Receiver{

  // 16 midi channels, 16 (possible) synths.
  private var activeSynths:Array[Option[ModularSynthesizer]] = Array.fill(16)(None)
  private var inputMidiDevice:Option[midi.MidiDevice] = None
  def buildOutput = ???

  /**
   *
   * @param msg
   * @param timestamp
   */
  def send(msg:MidiMessage, timestamp:Long) =
    // Superimpose resulting waves from our synths
    activeSynths.map(_.map(_.output).getOrElse(0.0)).sum

  /**
   * Change the device this uses
   * @return Unit
   */
  def setMidiInput() = ???
}
