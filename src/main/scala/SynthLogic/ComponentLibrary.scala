package SynthLogic

import SynthUtilities.*

import javax.sound.midi.{MidiMessage, ShortMessage}
import scala.util.{Success, Try}

object ComponentLibrary {

  // A list of tuples: first member is a name, second is a method that returns a synthcomponent by that name.
  private val components: Map[String, (ModularSynthesizer, String) => SynthComponent[Any]] = Map(
    /**
     * An identity operation. A component that passes through its input unchanged.
     */
    ("Passthrough",
      (host: ModularSynthesizer, serialID: String) =>
        PassThrough(host, Some(serialID))
    ),

    ("Oscillator",
      (host:ModularSynthesizer, serialID:String) =>
        Oscillator(host, Some(serialID))
    ),

    ("Amplifier",
      (host:ModularSynthesizer, serialID:String) =>
        Amplifier(host, Some(serialID))
    ),

    ("Average filter",
      (host: ModularSynthesizer, serialID: String) =>
        AvgFilter(host, Some(serialID))
    ),

    ("Average",
      (host: ModularSynthesizer, serialID: String) =>
        Average(host, Some(serialID))
    ),

    ("Sum",
      (host: ModularSynthesizer, serialID: String) =>
        Sum(host, Some(serialID))
    ),

    ("Product",
      (host: ModularSynthesizer, serialID: String) =>
        Sum(host, Some(serialID))
    ),

    /*
        ("Low-pass filter",
          (host: ModularSynthesizer, serialID: String) =>
            LowPass(host, Some(serialID))
        ),*/

    ("Delay",
      (host: ModularSynthesizer, serialID: String) =>
        Delay(host, Some(serialID))
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

  /**
   * A simple oscillator
   */
  class Oscillator(host:ModularSynthesizer,
                   override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    val oscillatorType:Parameter[Int] =
      new Parameter("type", "", true,  0, this) with EnumerableParam("sine", "square", "sawtooth", "noise")

    private var freq = 0.0
    // What part of the oscillator cycle are we on?
    private var phase = 0.0
    override def compute: Double =
      val msg = this.host.voice.message
      if msg.isDefined then
        if(msg.forall(_.getStatus == ShortMessage.NOTE_ON)) then
          freq = this.host.voice.message.map(SoundMath.noteFrequency).getOrElse(0.0)
      val ret =
        //println("Oscillator type: " + this.oscillatorType.value)
        this.oscillatorType.value match
          case 0 => MathUtilities.parametricSin(1, 0, phase, 0, 0)
          case 1 => MathUtilities.squareWave(1,phase)
          case 2 => MathUtilities.saw(1, phase)
          case 3 => MathUtilities.noise(1)
          case _ => 0
      phase = (phase + 2*math.Pi*freq*host.deltaTime)%(2*math.Pi)
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

  /*class LowPassFilter(host: ModularSynthesizer,
                  override val serializationTag: Option[String]) extends SynthComponent[Double](host):*/
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

  class Product(host: ModularSynthesizer,
            override val serializationTag: Option[String]) extends SynthComponent[Double](host):

    val input1: Parameter[Double] = Parameter[Double]("input 1", "", true, 0.0, this)
    val input2: Parameter[Double] = Parameter[Double]("input 2", "", true, 0.0, this)

    override def compute: Double =
      input1.value * input2.value


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

      val attackRate = 1.0 / attack.defaultValue
      val decayRate = 1.0 / decay.defaultValue
      val releaseRate = 1.0 / release.defaultValue

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

      previous = out
      out
  end Envelope

  // 1 if time from note start < time, 0 otherwise.
  class TimedEnvelope(host: ModularSynthesizer,
                 override val serializationTag: Option[String]) extends SynthComponent[Double](host):
    val sampleTime = Parameter[Int]("sample time", "", false, 1, this)

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
      if(time < sampleTime.value) then 1.0
      else
        active = false
        0.0 }
      time += 1
      out



}
