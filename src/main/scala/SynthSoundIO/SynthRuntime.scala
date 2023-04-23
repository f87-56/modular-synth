package SynthSoundIO
import SynthLogic.ModularSynthesizer
import SynthSoundIO.SynthRuntime.{BIT_DEPTH, BIT_RATE, BYTE_BUFFER_SIZE, SYNTH_BUFFER_SIZE}
import SynthUtilities.MathUtilities

import javax.sound.midi
import scala.concurrent.ExecutionContext.Implicits.global
import javax.sound.midi.{MidiDevice, MidiMessage, Receiver, ShortMessage, Transmitter}
import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, SourceDataLine}
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Try

/**
 * The "Executuion ground" for modular synthesizers. Handles passing information between the sound system and modular synthesizers.
 */
class SynthRuntime extends Receiver:

  // 16 midi channels, 16 (possible) synths.
  //private val activeSynths:Array[Option[ModularSynthesizer]] = Array.fill(16)(None)
  var activeSynth: ModularSynthesizer = ModularSynthesizer.default


  private val messageQueue:mutable.Queue[MidiMessage] = mutable.Queue()

  // TODO: Error handling
  // TODO: Remove magic constants
  // TODO: Configurability
  // Hopefully temporary. We just need to send audio now.
  val format = new AudioFormat(BIT_RATE, BIT_DEPTH, 1, true, false)
  //val info = new DataLine.Info(classOf[SourceDataLine], format)
  // format.issupported etc. etc.
  val line:SourceDataLine = AudioSystem.getSourceDataLine(format)
  println(line.getBufferSize)
  line.open(format, SynthRuntime.OUT_BUFFER_SIZE)
  println(line.getBufferSize)
  line.start()

  private var kill = false
  // All to-and-fro is to happen here.
  // Initialize thread, etc.
  def openOutput(): Future[Unit] =
    kill = false
    def writeToAudio(): Unit =
        val data = buildOutput(Try(Some(messageQueue.dequeue())).getOrElse(None))
        line.write(data, 0, BYTE_BUFFER_SIZE)

    /*
    Future(
      while !kill do
        writeToAudio()
    )*/
    val a = LazyList.continually(writeToAudio())
    Future(a.takeWhile(* => !kill).foreach(_ => ()))

      //println(buildOutput.mkString(","))
      //println("\n\n")

  def closeOutput() =
    kill = true

  def buildOutput(shortMessage:Option[MidiMessage]):Array[Byte] =
    Array.tabulate(SYNTH_BUFFER_SIZE)(
      _ => (MathUtilities.clamp(-1,1, activeSynth.output(shortMessage))
        *Short.MaxValue).toShort)
      .flatMap(a => MathUtilities.breakToBytes(a))

  // TODO: Make it run on another thread
  /**
   * TODO: Make this do something
   * @param msg
   * @param timestamp
   */
  def send(msg: MidiMessage, timestamp: Long): Unit =
    val status = msg.getStatus
    if(status == ShortMessage.NOTE_ON ||status == ShortMessage.NOTE_OFF) then
      messageQueue += msg

  def close(): Unit = line.close()

end SynthRuntime
object SynthRuntime:
  val SYNTH_BUFFER_SIZE: Int = 256
  val OUT_BUFFER_SIZE: Int = 2048
  val BIT_RATE: Int = 44100
  val BIT_DEPTH: Int = 16
  val BYTE_SIZE: Int = 8
  val BYTE_BUFFER_SIZE: Int = BIT_DEPTH / BYTE_SIZE * SYNTH_BUFFER_SIZE
end SynthRuntime
