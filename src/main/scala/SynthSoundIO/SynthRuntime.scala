package SynthSoundIO
import SynthLogic.ModularSynthesizer

import javax.sound.midi
import javax.sound.midi.{MidiDevice, MidiMessage, Receiver, Transmitter}
import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, SourceDataLine}

/**
 * The "Executuion ground" for modular synthesizers. Handles passing information between the sound system and modular synthesizers.
 */
class SynthRuntime extends Receiver{

  // 16 midi channels, 16 (possible) synths.
  //private val activeSynths:Array[Option[ModularSynthesizer]] = Array.fill(16)(None)
  val activeSynth = ModularSynthesizer.default
  private var inputMidiDevice:Option[midi.MidiDevice] = None

  private var time = 0

  private val BUFFER_SIZE = 256

  // Hopefully temporary. We just need to send audio now.
  // TODO: Error handling
  // TODO: Remove magic constants
  // TODO: Configurability
  val format = new AudioFormat(44100, 8, 1, true, false)
  //val info = new DataLine.Info(classOf[SourceDataLine], format)
  // format.issupported etc. etc.
  val line:SourceDataLine = AudioSystem.getSourceDataLine(format)
  line.open(format)
  line.start()

  // TODO: Include some way to close the data line on destruction.
  // line.close()
  //

  def buildOutput:Seq[Byte] =
    for(i <- 0 until BUFFER_SIZE) yield
      (activeSynth.output*Byte.MaxValue).toByte



  // TODO: Make it run on another thread
  /**
   * TODO: Make this do something
   * @param msg
   * @param timestamp
   */
  def send(msg: MidiMessage, timestamp: Long): Unit =
    ()
    // Superimpose resulting waves from our synths
    //activeSynths.map(_.map(_.output).getOrElse(0.0)).sum

  def close(): Unit = line.close()

  /**
   * Change the device this uses
   * @return Unit
   */
  def setMidiInput(midiDevice: MidiDevice) = inputMidiDevice = Some(midiDevice)
}
