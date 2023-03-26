package SynthSoundIO
import SynthLogic.ModularSynthesizer
import SynthUtilities.MathUtilities

import javax.sound.midi
import javax.sound.midi.{MidiDevice, MidiMessage, Receiver, ShortMessage, Transmitter}
import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, SourceDataLine}
import scala.collection.mutable

/**
 * The "Executuion ground" for modular synthesizers. Handles passing information between the sound system and modular synthesizers.
 */
class SynthRuntime extends Receiver:

  // 16 midi channels, 16 (possible) synths.
  //private val activeSynths:Array[Option[ModularSynthesizer]] = Array.fill(16)(None)
  val activeSynth: ModularSynthesizer = ModularSynthesizer.default

  private val BUFFER_SIZE = 256
  private val BIT_RATE = 44100
  private val BIT_DEPTH = 16
  private val BYTE_SIZE = 8
  private val BYTE_BUFFER_SIZE = BIT_DEPTH/BYTE_SIZE*BUFFER_SIZE

  private val messageQueue:mutable.Queue[MidiMessage] = mutable.Queue()

  // TODO: Error handling
  // TODO: Remove magic constants
  // TODO: Configurability
  // Hopefully temporary. We just need to send audio now.
  val format = new AudioFormat(BIT_RATE, BIT_DEPTH, 1, true, false)
  //val info = new DataLine.Info(classOf[SourceDataLine], format)
  // format.issupported etc. etc.
  val line:SourceDataLine = AudioSystem.getSourceDataLine(format)
  line.open(format)
  line.start()

  // All to-and-fro is to happen here.
  // Initialize thread, etc.
  def openOutput()=
    while(true) do
      //println(buildOutput.mkString(","))
      //println("\n\n")
      line.write(buildOutput(None), 0, BYTE_BUFFER_SIZE)


  def buildOutput(shortMessage:Option[ShortMessage]):Array[Byte] =
    (for(i <- 0 until BUFFER_SIZE) yield
      (activeSynth.output(shortMessage)*Short.MaxValue).toShort)
      .flatMap(a => MathUtilities.breakToBytes(a))
      .toArray

  // TODO: Make it run on another thread
  /**
   * TODO: Make this do something
   * @param msg
   * @param timestamp
   */
  def send(msg: MidiMessage, timestamp: Long): Unit =
    messageQueue += msg
    // Superimpose resulting waves from our synths
    //activeSynths.map(_.map(_.output).getOrElse(0.0)).sum

  def close(): Unit = line.close()

end SynthRuntime