package SynthSoundIO
import javax.sound.midi.{MidiDeviceReceiver, MidiDeviceTransmitter, MidiSystem, Receiver, Synthesizer, Transmitter}
import scala.util.{Success, Try}

object AudioResourceHandler {
  
  // The canonical runtime
  val defaultRuntime = SynthRuntime()
  // The "canonical instance of the keyboard control.
  val keyboardControl = KeyboardMidiControl(Some(defaultRuntime))

  /**
   * @return returns all the available midi input devices.
   */
  def MIDIInputs =
    val deviceInfo = MidiSystem.getMidiDeviceInfo
    val allReceivers = deviceInfo.map(a => Try(MidiSystem.getMidiDevice(a).getTransmitter))
    //val res = keyboardControl +: allDevices.filter(_.isInstanceOf[Receiver])
    val res = keyboardControl +: allReceivers.collect{case Success(a:MidiDeviceTransmitter) => a}
    res

  /**
   * Sets the synth runtime this device sends to.
   * @param transmitter
   * @param synthRuntime
   * @return
   */
  def setSynthRuntime(transmitter: MidiDeviceTransmitter, synthRuntime: SynthRuntime) =
    Try{transmitter.getMidiDevice.open()
      transmitter.setReceiver(synthRuntime)}

  /**
   * Free this transmitter for the use of other applications.
   * @param transmitter
   * @param synthRuntime
   * @return
   */
  def freeTransmitter(transmitter: MidiDeviceTransmitter) =
    Try{
      transmitter.getMidiDevice.close()
    }
}
