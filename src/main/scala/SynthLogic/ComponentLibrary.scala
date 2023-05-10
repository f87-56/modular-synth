package SynthLogic

import SynthUtilities.*
import com.sun.javafx.binding.DoubleConstant

import javax.imageio.spi.ServiceRegistry.Filter
import javax.sound.midi.{MidiMessage, ShortMessage}
import scala.util.{Success, Try}

object ComponentLibrary {

  // A list of tuples: first member is a name, second is a method that returns a synthcomponent by that name.
  private val components: Map[String, (ModularSynthesizer, String) => SynthComponent[_]] = Map(
    /**
     * An identity operation. A component that passes through its input unchanged.
     */
    ("Passthrough",
      (host: ModularSynthesizer, serialID: String) =>
        PassThrough(host, Some(serialID))
    ),

    ("Probe",
      (host: ModularSynthesizer, serialID: String) =>
        Probe(host, Some(serialID))
    ),

    ("Oscillator",
      (host:ModularSynthesizer, serialID:String) =>
        Oscillator(host, Some(serialID))
    ),

    ("Note frequency",
      (host: ModularSynthesizer, serialID: String) =>
        NoteFrequency(host, Some(serialID))
    ),

    ("Note velocity",
      (host: ModularSynthesizer, serialID: String) =>
        NoteVelocity(host, Some(serialID))
    ),

    ("Amplifier",
      (host:ModularSynthesizer, serialID:String) =>
        Amplifier(host, Some(serialID))
    ),

    ("Average filter",
      (host: ModularSynthesizer, serialID: String) =>
        AvgFilter(host, Some(serialID))
    ),

    ("IIR low-pass filter",
      (host: ModularSynthesizer, serialID: String) =>
        LowPassIIR(host, Some(serialID))
    ),

    ("RC high-pass filter",
      (host: ModularSynthesizer, serialID: String) =>
        HighPassRC(host, Some(serialID))
    ),

    ("RC low-pass filter",
      (host: ModularSynthesizer, serialID: String) =>
        LowPassRC(host, Some(serialID))
    ),

    ("Average",
      (host: ModularSynthesizer, serialID: String) =>
        Average(host, Some(serialID))
    ),

    ("Sum",
      (host: ModularSynthesizer, serialID: String) =>
        Sum(host, Some(serialID))
    ),

    ("Divide",
      (host: ModularSynthesizer, serialID: String) =>
        Divide(host, Some(serialID))
    ),

    ("Product",
      (host: ModularSynthesizer, serialID: String) =>
        Sum(host, Some(serialID))
    ),

    ("Delay",
      (host: ModularSynthesizer, serialID: String) =>
        Delay(host, Some(serialID))
    ),

    ("Double -> Int",
      (host: ModularSynthesizer, serialID: String) =>
        DoubleToInt(host, Some(serialID))
    ),

    ("Int -> Double",
      (host: ModularSynthesizer, serialID: String) =>
        IntToDouble(host, Some(serialID))
    ),

    ("Array delay",
      (host: ModularSynthesizer, serialID: String) =>
        ArrayDelay(host, Some(serialID))
    ),

    ("TestStringComponent",
      (host:ModularSynthesizer, serialID:String) =>
        TestComp(host, Some(serialID))
    ),

    ("Envelope",
      (host:ModularSynthesizer, serialID:String) =>
        Envelope(host, Some(serialID))
    ),

    ("Timed envelope",
      (host: ModularSynthesizer, serialID: String) =>
        TimedEnvelope(host, Some(serialID))
    ),

    ("Constant",
      (host: ModularSynthesizer, serialID: String) =>
        ConstDouble(host, Some(serialID))
    )

  )

  def componentNames: Iterable[String] = components.keys
  def createComponent(name:String,host:ModularSynthesizer): Option[SynthComponent[_]] =
    components.get(name).map(_(host, name))


  /**
   * An identity operation. A component that passes through its input unchanged.
   */
  class PassThrough(host: ModularSynthesizer,
                    override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    val input: Parameter[Double] = Parameter("input", "", true, 0.5, this)

    // I am fairly satisfied as to how this looks.
    override def compute: Double =
      input.value

  // A probe node for easy debugging
  class Probe(host: ModularSynthesizer,
                    override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    val input: Parameter[Double] = Parameter("input", "", true, 0.5, this)

    override def compute: Double =
      println("Probe " + this.toString + ": " + input.value)
      input.value


  class NoteFrequency(host: ModularSynthesizer,
                   override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    private var freq = 0.0
    override def compute: Double =
      val msg = this.host.voice.message
      if msg.isDefined then
        if (msg.forall(_.getStatus == ShortMessage.NOTE_ON)) then
          freq = this.host.voice.message.map(SoundMath.noteFrequency).getOrElse(0.0)
      freq

  class NoteVelocity(host: ModularSynthesizer,
                      override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    private val MAX_VEL: Double = 127.0
    private var vel = 0.0

    override def compute: Double =
      val msg = this.host.voice.message
      if (msg.forall(_.getStatus == ShortMessage.NOTE_ON)) then
        val msgVelocity = msg.flatMap(_.getMessage.lift(2))
        msgVelocity.foreach(a => vel = a/MAX_VEL)
      vel

  /**
   * A simple oscillator
   */
  class Oscillator(host:ModularSynthesizer,
                   override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    private val oscillatorType:Parameter[Int] =
      new Parameter("type", "", true,  0, this) with EnumerableParam("sine", "square", "sawtooth", "noise")

    // For FM
    val frequency: Parameter[Double] = Parameter[Double]("frequency", "", true, 1, this)

    private var freq = 0.0
    // What part of the oscillator cycle are we on?
    private var phase = 0.0
    override def compute: Double =
      val ret =
        //println("Oscillator type: " + this.oscillatorType.value)
        this.oscillatorType.value match
          case 0 => MathUtilities.parametricSin(1, 0, phase, 0, 0)
          case 1 => MathUtilities.squareWave(1,phase)
          case 2 => MathUtilities.saw(1, phase)
          case 3 => MathUtilities.noise(1)
          case _ => 0
      phase = (phase + 2*math.Pi*host.deltaTime*frequency.value)%(2*math.Pi)
      ret

  /**
   * Scales the input signal by the parameter gain.
   */
  class Amplifier(host:ModularSynthesizer,
                  override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val input: Parameter[Double] = Parameter[Double]("input", "", true, 1.0, this)
    val gain: Parameter[Double] = Parameter[Double]("gain", "", true, 1.0, this)
    def compute: Double =
      input.value * gain.value

  class AvgFilter(host: ModularSynthesizer,
                  override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val bufferSize: Parameter[Int] = new Parameter[Int]("Buffer size", "", false, 1, this):
      override def defaultValue_=(newVal: Any): Unit =
        newVal match
          case a:Int =>
            if(a >= 1) then
              super.defaultValue_=(newVal)
              prevValBuffer = SynthUtilities.MaxSizeQueue[Double](this.value, 0.0)
          case _ => ()

    val input: Parameter[Double] = Parameter[Double]("input", "", true, 0.0, this)

    private var prevValBuffer = SynthUtilities.MaxSizeQueue[Double](bufferSize.value, 0.0)
    override def compute: Double =
      prevValBuffer.append(this.input.value)
      val out = prevValBuffer.values.sum/prevValBuffer.size
      //prevValBuffer.append(out)
      out

  // TODO: Document this properly
  // Works somehow, parameters a bit sketchy. Can acquire unusable state.
  // https://arachnoid.com/BiQuadDesigner/index.html
  // https://github.com/philburk/listenup/blob/master/src/com/softsynth/dsp/BiquadFilter.java
  class LowPassIIR(host: ModularSynthesizer,
                  override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val input: Parameter[Double] = Parameter[Double]("input", "", true, 0.0, this)

    val cutoff: Parameter[Double] = new Parameter[Double]("cutoff", "", true, 4000, this):
      override def defaultValue_=(newVal: Any): Unit =
        newVal match
          case a:Double =>
              super.defaultValue_=(newVal)
              calculateCoefficients()
          case _ => ()

    val resonance: Parameter[Double] = new Parameter[Double]("resonance", "", true, 1.0, this):
      override def defaultValue_=(newVal: Any): Unit =
        newVal match
          case a: Double =>
            super.defaultValue_=(newVal)
            calculateCoefficients()
          case _ => ()

    // filter coefficients
    private var b0, b1, b2, a1, a2 = 0.0
    // internal state
    private var w1, w2 = 0.0
    calculateCoefficients()

    private def calculateCoefficients(): Unit =
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


    def filter(x:Double): Double =
      val y = b0 * x + w1
      w1 = b1 * x + w2 - a1 * y
      w2 = b2 * x - a2 * y
      y
    override def compute: Double =
      filter(input.value)

  end LowPassIIR

  class HighPassRC(host: ModularSynthesizer,
                   override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    val input: Parameter[Double] = Parameter[Double]("input", "", true, 0.0, this)
    val f_c: Parameter[Double] = Parameter[Double]("attenuation frequency", "", true, 0.0, this)

    // State of filter
    private var yPrev = 0.0
    private var xPrev = 0.0

    override def compute: Double =
      val dt = 1.0/host.sampleRate
      val alpha = 1 / (2*math.Pi*dt* math.max(f_c.value,0) + 1)
      val out = alpha * yPrev + alpha*(input.value - xPrev)
      xPrev = input.value
      yPrev = out
      yPrev


  class LowPassRC(host: ModularSynthesizer,
                   override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    val input: Parameter[Double] = Parameter[Double]("input", "", true, 0.0, this)
    val f_c: Parameter[Double] = Parameter[Double]("attenuation frequency", "", true, 0.0, this)

    // State of filter
    private var yPrev = 0.0
    private var xPrev = 0.0

    override def compute: Double =
      val dt = 1.0 / host.sampleRate
      val alpha = (2*math.Pi*dt*math.max(f_c.value, 0)) / (2 * math.Pi * dt * math.max(f_c.value, 0) + 1)
      val out = alpha * input.value + (1.0-alpha) * (yPrev)
      xPrev = input.value
      yPrev = out
      yPrev


  class Delay(host: ModularSynthesizer,
                      override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    val input: Parameter[Double] = Parameter[Double]("input", "", true, 0.0, this)

    val bufferSize: Parameter[Int] = new Parameter[Int]("Buffer size", "", false, 1, this):
      override def defaultValue_=(newVal: Any): Unit =
        newVal match
          case a:Int =>
            if(a >= 1) then
              super.defaultValue_=(newVal)
              prevValBuffer = SynthUtilities.MaxSizeQueue[Double](this.value, 0.0)
          case _ => ()


    private var prevValBuffer = SynthUtilities.MaxSizeQueue[Double](bufferSize.value, 0.0)
    override def compute: Double =
      val out = Try(prevValBuffer.dequeue())
      val in = this.input.value
      prevValBuffer.append(in)
      out.getOrElse(0.0)

  // Implemented with buffer, "variable size"
  class ArrayDelay(host: ModularSynthesizer,
              override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    // Max delay in samples
    private val MaxSize = 100000
    // One writes, one reads
    private var writeCursor = 0

    val input: Parameter[Double] = Parameter[Double]("input", "", true, 0.0, this)
    val bufferSize: Parameter[Int] = new Parameter[Int]("Buffer size", "", true, 1, this):
      override def defaultValue_=(newVal: Any): Unit =
        newVal match
          case a: Int =>
            super.defaultValue_=(math.max(a,1))
          case _ => ()


    // We only use the n first values (n being the delay)
    private val prevValBuffer = Array.ofDim[Double](MaxSize)
    override def compute: Double =
      val in = this.input.value
      //val size = math.max(bufferSize.value,1)
      val size = math.max(bufferSize.value,1)
      Try(prevValBuffer(writeCursor) = in)
      // Next position of the write cursor
      writeCursor = ((writeCursor + 1) % (size)) % (MaxSize)
      val ret = prevValBuffer.lift(writeCursor)
      ret.getOrElse(0.0)

  end ArrayDelay

  class Average(host: ModularSynthesizer,
                 override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val input1: Parameter[Double] = Parameter[Double]("input 1", "", true, 0.0, this)
    val input2: Parameter[Double] = Parameter[Double]("input 2", "", true, 0.0, this)

    override def compute: Double =
      (input1.value + input2.value)/2

  class Sum(host: ModularSynthesizer,
                override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val input1: Parameter[Double] = Parameter[Double]("input 1", "", true, 0.0, this)
    val input2: Parameter[Double] = Parameter[Double]("input 2", "", true, 0.0, this)

    override def compute: Double =
      val out = input1.value + input2.value
      out


  class Divide(host: ModularSynthesizer,
            override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val input1: Parameter[Double] = Parameter[Double]("input 1", "", true, 0.0, this)
    val input2: Parameter[Double] = Parameter[Double]("input 2", "", true, 0.0, this)

    override def compute: Double =
      val out = {
      if !(input2.value == 0) then
        input1.value / input2.value
      else 0.0
      }
      out

  class Product(host: ModularSynthesizer,
            override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val input1: Parameter[Double] = Parameter[Double]("input 1", "", true, 0.0, this)
    val input2: Parameter[Double] = Parameter[Double]("input 2", "", true, 0.0, this)

    override def compute: Double =
      input1.value * input2.value

  class DoubleToInt(host: ModularSynthesizer,
            override val serializationTag: Option[String]) extends SynthComponent[Int](host):

    val input: Parameter[Double] = Parameter[Double]("input", "", true, 0.0, this)

    override def compute: Int =
      input.value.toInt

  class IntToDouble(host: ModularSynthesizer,
                    override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val input: Parameter[Int] = Parameter[Int]("input", "", true, 0, this)

    override def compute: Double =
      input.value.toDouble


  // A constant
  class ConstDouble(host: ModularSynthesizer,
                override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val doubleValue: Parameter[Double] = Parameter[Double]("value", "", true, 0.0, this)
    override def compute: Double =
      doubleValue.value


  class TestComp(host: ModularSynthesizer,
                 override val serializationTag: Option[String]) extends SynthComponent[String](host):

    val input: Parameter[String] = Parameter[String]("gain", "", true, "Zero", this)
    val gain: Parameter[String] = Parameter[String]("input", "", true, "One", this)
    def compute: String =
      input.value + gain.value


  // Provides a time-varying multiplier, from 0 to 1.
  class Envelope(host:ModularSynthesizer,
                 override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    val attack = Parameter[Double]("attack", "", false, 0.1, this)
    val decay = Parameter[Double]("decay", "", false, 0.1, this)
    val sustain = Parameter[Double]("sustain", "", false, 0.1, this)
    val release = Parameter[Double]("release", "", false, 0.1, this)

    enum State:
      case Attack, DecaySustain, Release, Dead

  // What "phase" are we in?
    private var state = State.Dead
    // When did the phase start?
    private var stateStartTime = 0.0
    private var previous = 0.0
    private var mostRecentMsg:Option[MidiMessage] = None

    // A trapezoidal model. The method is a bit of a mess.
    override def compute: Double =

      val attackRate = 1.0 / math.max(attack.defaultValue,0.0001)
      val decayRate = 1.0 / math.max(decay.defaultValue, 0.0001)
      val releaseRate = 1.0 / math.max(release.defaultValue, 0.0001)

      val time = SoundMath.sampleToTime(host.voice.sample, host.voice.sampleRate)
      val deltaTime = SoundMath.sampleToTime(1, host.voice.sampleRate)

      val msg = host.voice.message
      val messageStatus = msg.map(_.getStatus)
      val msgVelocity = msg.flatMap(_.getMessage.lift(2))
      // Update the messageStatus
      // A note has started at this moment
      if messageStatus.contains(ShortMessage.NOTE_ON ) && msgVelocity.exists(_!= 0) then
        mostRecentMsg = msg
        state = State.Attack
        stateStartTime = time
      // We've just released a key
      // Some devices use a NOTE_ON with velocity 0 to signal a NOTE_OFF
      else if (messageStatus.contains(ShortMessage.NOTE_OFF) ||
        messageStatus.contains(ShortMessage.NOTE_ON)) &&
        mostRecentMsg.map(SoundMath.noteFrequency) == msg.map(SoundMath.noteFrequency)
        && state != State.Release && state != State.Dead then
          state = State.Release
          stateStartTime = time
      // Attack is over, move to decay.
      else if state == State.Attack && previous >= 1 then
        state = State.DecaySustain
        stateStartTime = time
      // Decay is over, mark as dead.
      else if state == State.Release && previous <= 0 then
        state = State.Dead
        stateStartTime = time

      val out = state match
        case State.Attack => previous + deltaTime * attackRate
        case State.DecaySustain =>
          if(previous <= sustain.defaultValue) then previous
          else previous - deltaTime * decayRate
        case State.Release => previous - deltaTime * releaseRate
        case State.Dead => 0.0

      previous = MathUtilities.clamp(0.0,1.0,out)
      previous
  end Envelope

  // 1 if time from note start < time, 0 otherwise.
  class TimedEnvelope(host: ModularSynthesizer,
                 override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    val sampleTime = Parameter[Int]("sample time", "", true, 1, this)

    private var time = 0
    private var active = false
    override def compute: Double =

      val msg = host.voice.message
      val messageStatus = msg.map(_.getStatus)
      val msgVelocity = msg.flatMap(_.getMessage.lift(2))

      if messageStatus.contains(ShortMessage.NOTE_ON) && msgVelocity.exists(_ != 0) then
        time = 0
        active = true

      val out = {
      if(time < sampleTime.value && active) then 1.0
      else
        active = false
        0.0 }
      time += 1
      out



}
