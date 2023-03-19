package SynthLogic

import SynthUtilities.*

import javax.sound.midi.ShortMessage

object ComponentLibrary {

  /**
   * An identity operation. A component that passes through its input unchanged.
   */
  class PassThrough extends SynthComponent[DoubleSignal]():
    val parameters: Seq[Parameter[DoubleSignal]] =
      Vector(Parameter[DoubleSignal]("input","", true, DoubleSignal(0.5), this))

    // I am fairly satisfied as to how this looks.
    override def output(runtimeContext: RuntimeContext) =
      paramValue[DoubleSignal]("input", runtimeContext).getOrElse(DoubleSignal(0.0))

  /**
   * A simple oscillator
   */
  class Oscillator extends SynthComponent[DoubleSignal]():
    override val parameters: Seq[Parameter[SignalType]] =
      Vector(
        new Parameter[IntSignal]("type", "", false,  IntSignal(0),this) with EnumerableParam("sine", "square", "sawtooth", "noise")
      )
    override def output(rc: RuntimeContext) =
      DoubleSignal(MathUtilities.parametricSin(1,2*math.Pi*440,0,0,
        SoundUtilities.sampleToTime(rc.sample, rc.sampleRate)))

  /**
   * Scales the input signal by the parameter gain.
   */
  class Amplifier extends SynthComponent[DoubleSignal]():

    val input = Parameter[DoubleSignal]("gain", "", true, DoubleSignal(1), this)
    val gain = Parameter[DoubleSignal]("input", "", true, DoubleSignal(1), this)
    override val parameters: Seq[Parameter[SignalType]] =
      Vector(input, gain)
    def output(rc:RuntimeContext) =
      DoubleSignal(input.value(rc).value * gain.value(rc).value)


  // Provides a time-varying multiplier, from 0 to 1.
  class Envelope extends SynthComponent[DoubleSignal]():
    val attack = Parameter[DoubleSignal]("attack", "", false, DoubleSignal(0.1), this)
    val decay = Parameter[DoubleSignal]("decay", "", false, DoubleSignal(2), this)
    val sustain = Parameter[DoubleSignal]("sustain", "", false, DoubleSignal(0.2), this)
    val release = Parameter[DoubleSignal]("release", "", false, DoubleSignal(2), this)

    val attackRate = 1.0/attack.defaultValue.value
    val decayRate = 1.0/decay.defaultValue.value
    val releaseRate = 1.0/release.defaultValue.value

    override val parameters: Seq[Parameter[SignalType]] =
      Vector(
        attack, decay, sustain, release
      )

    enum State:
      case Attack, DecaySustain, Release, Dead

  // What "phase" are we in?
    private var state = State.Attack
    // When did the phase start?
    private var stateStartTime = 0.0
    // the previous value
    private var prevValue = 0.0

    // A trapezoidal model. The method is a bit of a mess.
    override def output(rc:RuntimeContext): DoubleSignal =
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
          if(prevValue <= sustain.defaultValue.value) then prevValue
          else prevValue - deltaTime * decayRate
        case State.Release => prevValue - deltaTime * releaseRate
        case State.Dead => 0.0

      prevValue = out
      DoubleSignal(out)
  end Envelope

  def passthrough: PassThrough = PassThrough()

}
