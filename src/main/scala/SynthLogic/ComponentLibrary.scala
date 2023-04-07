package SynthLogic

import SynthUtilities.*

import javax.sound.midi.{MidiMessage, ShortMessage}

object ComponentLibrary {

  /**
   * An identity operation. A component that passes through its input unchanged.
   */
  class PassThrough(host:ModularSynthesizer) extends SynthComponent[Double](host):
    val input: Parameter[Double] = Parameter("input","", true, 0.5, this)

    // I am fairly satisfied as to how this looks.
    override def compute: Double =
      input.value

  /**
   * A simple oscillator
   */
  class Oscillator(host:ModularSynthesizer) extends SynthComponent[Double](host):
    val oscillatorType:Parameter[Int] =
      new Parameter("type", "", false,  0, this) with EnumerableParam("sine", "square", "sawtooth", "noise")

    private var freq = 0.0
    // What part of the oscillator cycle are we on?
    private var phase = 0.0
    override def compute: Double =
      val msg = this.host.voice.message
      if msg.isDefined then
        if(msg.forall(_.getStatus == ShortMessage.NOTE_ON)) then
          freq = this.host.voice.message.map(SoundMath.noteFrequency).getOrElse(0.0)
      val ret = MathUtilities.parametricSin(1, 0, phase, 0, 0)
      phase = (phase + 2*math.Pi*freq*host.deltaTime)%(2*math.Pi)
      ret

  /**
   * Scales the input signal by the parameter gain.
   */
  class Amplifier(host:ModularSynthesizer) extends SynthComponent[Double](host):

    val input: Parameter[Double] = Parameter[Double]("gain", "", true, 1.0, this)
    val gain: Parameter[Double] = Parameter[Double]("input", "", true, 1.0, this)
    def compute: Double =
      input.value * gain.value


  // Provides a time-varying multiplier, from 0 to 1.
  class Envelope(host:ModularSynthesizer) extends SynthComponent[Double](host):
    val attack = Parameter[Double]("attack", "", false, 0.1, this)
    val decay = Parameter[Double]("decay", "", false, 0.1, this)
    val sustain = Parameter[Double]("sustain", "", false, 0.1, this)
    val release = Parameter[Double]("release", "", false, 0.1, this)

    private val attackRate = 1.0/attack.defaultValue
    private val decayRate = 1.0/decay.defaultValue
    private val releaseRate = 1.0/release.defaultValue

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

  def passthrough(host:ModularSynthesizer): PassThrough = PassThrough(host)

}
