package SynthLogic

import SynthUtilities.*

import javax.sound.midi.ShortMessage

object ComponentLibrary {

  /**
   * An identity operation. A component that passes through its input unchanged.
   */
  class PassThrough extends SynthComponent[Double]():
    val input: Parameter[Double] = Parameter("input","", true, 0.5, this)

    // I am fairly satisfied as to how this looks.
    override def output(runtimeContext: RuntimeContext) =
      input.value(runtimeContext)

  /**
   * A simple oscillator
   */
  class Oscillator extends SynthComponent[Double]():
    val oscillatorType:Parameter[Int] =
      new Parameter("type", "", false,  0,this) with EnumerableParam("sine", "square", "sawtooth", "noise")
    override def output(rc: RuntimeContext) =
      MathUtilities.parametricSin(1,2*math.Pi*440,0,0,
        SoundUtilities.sampleToTime(rc.sample, rc.sampleRate))

  /**
   * Scales the input signal by the parameter gain.
   */
  class Amplifier extends SynthComponent[Double]():

    val input = Parameter[Double]("gain", "", true, 1.0, this)
    val gain = Parameter[Double]("input", "", true, 1.0, this)
    def output(rc:RuntimeContext) =
      input.value(rc) * gain.value(rc)


  // Provides a time-varying multiplier, from 0 to 1.
  class Envelope extends SynthComponent[Double]():
    val attack = Parameter[Double]("attack", "", false, 0.3, this)
    val decay = Parameter[Double]("decay", "", false, 2, this)
    val sustain = Parameter[Double]("sustain", "", false, 0.1, this)
    val release = Parameter[Double]("release", "", false, 2, this)

    val attackRate = 1.0/attack.defaultValue
    val decayRate = 1.0/decay.defaultValue
    val releaseRate = 1.0/release.defaultValue

    enum State:
      case Attack, DecaySustain, Release, Dead

  // What "phase" are we in?
    private var state = State.Attack
    // When did the phase start?
    private var stateStartTime = 0.0
    // the previous value
    private var prevValue = 0.0

    // A trapezoidal model. The method is a bit of a mess.
    override def output(rc:RuntimeContext): Double =
      val time = SoundUtilities.sampleToTime(rc.sample, rc.sampleRate)
      val deltaTime = SoundUtilities.sampleToTime(1, rc.sampleRate)

      val messageStatus = rc.message.map(_.getStatus)

      // Update the messageStatus
      // A note has started at this moment
      if messageStatus.contains(ShortMessage.NOTE_ON) then
        state = State.Attack
        stateStartTime = time
      // We've just released a key
      else if messageStatus.contains(ShortMessage.NOTE_OFF) && state != State.Release && state != State.Dead then
        state = State.DecaySustain
        stateStartTime = time
      // Attack is over, move to decay.
      else if state == State.Attack && prevValue >= 1 then
        state = State.DecaySustain
        stateStartTime = time
      // Decay is over, mark as dead.
      else if state == State.Release && prevValue <= 0 then
        state = State.Dead
        stateStartTime = time

      val out = state match
        case State.Attack => prevValue + deltaTime * attackRate
        case State.DecaySustain =>
          if(prevValue <= sustain.defaultValue) then prevValue
          else prevValue - deltaTime * decayRate
        case State.Release => prevValue - deltaTime * releaseRate
        case State.Dead => 0.0

      prevValue = out
      out
  end Envelope

  def passthrough: PassThrough = PassThrough()

}
