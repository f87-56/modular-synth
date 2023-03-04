import math.*

object SoundUtilities:

  private val MaxMidiNoteNum = 127
  private val MinMidiNoteNum = 0

  def dbToVolume(dB:Double) =
    math.pow(10.0, 0.05*dB)
  // Observe: This function can return negativeInfinity.
  def volumeTodB(volume:Double) =
    require(volume >= 0)
    20.0 * math.log10(volume)

  /**
   *
   * @param MidiNumber Gets clamped into the range [0, 127], which conforms to the standard.
   * @return
   */
  def noteFrequency(MidiNumber:Int) =
    // Clamp input range
    val clampedNum = MathUtilities.clamp(MinMidiNoteNum, MaxMidiNoteNum, MidiNumber)
    NoteFrequencies.getOrElse(clampedNum, 0.0)

  // midi codes of middle octave to their corresponding frequencies
  private val MiddleA = (69 -> 440.0)

  // Courtesy of https://newt.phys.unsw.edu.au/jw/notes.html
  private def genNoteFrequency(MidiNumber:Int) =
    val cDiff = MidiNumber - MiddleA._1
    val f = math.pow(2, cDiff/12.0)*MiddleA._2
    f

  // Maps midi numbers to theier corresponding frequencies
  private val NoteFrequencies:Map[Int, Double] =
    (MinMidiNoteNum to MaxMidiNoteNum)
      .zipWithIndex
      .map(a => (a._2 -> genNoteFrequency(a._1))).toMap




object MathUtilities:

  def clamp(lower:Int, upper:Int, value:Int) =
    math.min(math.max(lower,value), upper)
