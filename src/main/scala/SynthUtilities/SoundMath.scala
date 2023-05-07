import scala.math.*

package SynthUtilities:

  import SynthLogic.ComponentLibrary.Amplifier

  import java.nio.DoubleBuffer
  import javax.sound.midi.MidiMessage
  import scala.collection.mutable

  object SoundMath:

    private val MaxMidiNoteNum = 127
    private val MinMidiNoteNum = 0

    def dbToVolume(dB:Double): Double =
      math.pow(10.0, 0.05*dB)
    // Observe: This function can return negativeInfinity.
    def volumeTodB(volume:Double): Double =
      require(volume >= 0)
      20.0 * math.log10(volume)


    /**
     *
     * @param midiNumber Gets clamped into the range [0, 127], which conforms to the standard.
     * @return
     */
    def noteFrequency(midiNumber:Int): Double =
      // Clamp input range
      val clampedNum = MathUtilities.clamp(MinMidiNoteNum, MaxMidiNoteNum, midiNumber)
      NoteFrequencies.getOrElse(clampedNum, 0.0)

    def noteFrequency(midiMessage: MidiMessage):Double =
      noteFrequency(midiMessage.getMessage.lift(1).getOrElse(0.toByte))

    def sampleToTime(sampleNum:Int, sampleRate:Int): Double = sampleNum/(sampleRate.toDouble)

    // midi codes of middle octave to their corresponding frequencies
    private val MiddleA = 69 -> 440.0

    // Courtesy of https://newt.phys.unsw.edu.au/jw/notes.html
    private def genNoteFrequency(MidiNumber:Int) =
      val cDiff = MidiNumber - MiddleA._1
      val f = math.pow(2, cDiff/12.0)*MiddleA._2
      f

    // Maps midi numbers to theier corresponding frequencies
    private val NoteFrequencies:Map[Int, Double] =
      (MinMidiNoteNum to MaxMidiNoteNum)
        .zipWithIndex
        .map(a => a._2 -> genNoteFrequency(a._1)).toMap

  end SoundMath



  object MathUtilities:
    def clamp(lower:Int, upper:Int, value:Int): Int =
      math.min(math.max(lower,value), upper)

    def clamp(lower: Double, upper: Double, value: Double): Double =
      math.min(math.max(lower, value), upper)

    def parametricSin(amplitude:Double, angularVelocity:Double, phase:Double, offset:Double, x:Double): Double =
      amplitude*math.sin(angularVelocity*x - phase)+offset

    def parametricCos(amplitude:Double, angularVelocity:Double, phase:Double, offset:Double, x:Double): Double =
      parametricSin(amplitude, angularVelocity:Double, phase+Math.PI/2, offset, x:Double)

    def saw(amplitude:Double, phase:Double): Double =
      lerp(-amplitude,amplitude,(phase/(2*Pi))%1)

    def squareWave(amplitude:Double, phase:Double): Double =
      if(phase%(2*Pi) <= Pi) then 1.0 else -1.0

    // (white?) noise based on scala.util.Random
    def noise(amplitude:Double) =
      scala.util.Random.between(-amplitude,amplitude)


    def lerp(a:Double, b:Double, alpha:Double) =
      val cAlpha = clamp(0,1,alpha)
      a * cAlpha + b*(1-alpha)

    // Break the short into two, separate bytes.
    def breakToBytes(s:Short):Seq[Byte] =
      Seq(s.toByte, (s >>> 8).toByte)

  end MathUtilities

  object LogUtilities:

    // from http://biercoff.com/easily-measuring-code-execution-time-in-scala/
    def logTime[R](block: => R):R =
      val t0 = System.nanoTime()
      val result = block
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) + "ns")
      result

  end LogUtilities

  class MaxSizeQueue[A](val size:Int, val fillVal:A):
    // Initialize
    private val internalQueue = mutable.Queue.tabulate(size)(a => fillVal)

    // This may be subject to optimization
    def values: Seq[A] = internalQueue.toVector

    // Add value to be last, remove first if size exceeded.
    def append(value:A):Unit =
      internalQueue += value
      if internalQueue.size > size then
        internalQueue.dequeue()
        
    // WARNING: THROWS
    def dequeue(): A =
      internalQueue.dequeue()


  // Straight from (Apache licence 2.0)
  /*
  trait BiquadFilter:
    // filter coefficients
    private var b0, b1, b2, a1, a2 = 0.0
    // internal state
    private var x1, x2, y1, y2 = 0.0
    calculateCoefficients()

    // Calculate coefficients common to all types of filters
    private def calculateCommon(): Unit =
      val g = math.tan(math.Pi * cutoff.value)
      val r = 1.0 / (1.0 + g * (g + resonance.value))
      b0 = g * g * r
      b1 = 2.0 * b0
      b2 = b0
      a1 = 2.0 * r * (g * g - 1.0)
      a2 = r * (1.0 - g * (g - resonance.value))
      println("---------")
      println(this.a1)
      println(this.a2)
      println(this.b0)
      println(this.b1)
      println(this.b2)
      println("-----------")


    def filter(x: Double): Double =
      val y = b0 * x + w1
      w1 = b1 * x + w2 - a1 * y
      w2 = b2 * x - a2 * y
      y

  end BiquadFilter
*/