import scala.math
import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, SourceDataLine}

object Main extends App {

  // This is all placeholder
  val bitBeat = (
    for (t <- 1 to 20000000)
      yield
        //val a= (((t << 1) ^ ((t << 1) + (t >> 7) & t >> 12)) | t >> (4 - (1 ^ 7 & (t >> 19))) | t >> 7).toByte
        // Sine
        val a = (127.0f*math.sin(440*2.0f*Math.PI*t/44100.0f)).toByte
        // Square
        //val a = (2 * 127 * ((880 * t / (2 * 44100)) % 2) - 127).toByte
        // Sawtooth
        //val a = (127.0*2*(440*t/(44100.0)-math.floor(1/2.0+440*t/44100.0))).toByte
        a
    ).toArray

  val format = new AudioFormat(44100, 8, 1, true, false)
  val info = new DataLine.Info(classOf[SourceDataLine], format)
  val line = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
  line.open(format)
  line.start()
  line.write(bitBeat, 0, bitBeat.length)
  line.drain()
  line.close()

  // and this small change makes it run in a background thread
  //Prepare the beat
  val beat = new Runnable {
    override def run() = {
      line.start()
      line.write(bitBeat, 0, bitBeat.length)
      line.drain()
      line.close()
    }
  }
  // Start playback
  new Thread(beat).start()
}
