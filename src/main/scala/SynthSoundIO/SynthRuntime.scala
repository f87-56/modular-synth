package SynthSoundIO
import javax.sound.midi

/**
 * The "Executuion ground" for modular synthesizers. Handles passing information between the sound system and modular synthesizers.
 */
class SynthRuntime {

  private var inputMidiDevice:Option[midi.MidiDevice] = None
  def buildOutput = ???
  def send() = ???
}
