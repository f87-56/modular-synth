package SynthSoundIO
import javax.sound.midi.{MidiDeviceReceiver, MidiDeviceTransmitter, MidiSystem, Receiver, Synthesizer, Transmitter}
import scala.util.{Success, Try}

object AudioResourceHandler {
  
  // The canonical runtime
  val defaultRuntime: SynthRuntime = SynthRuntime()
  // The "canonical instance of the keyboard control.
  val keyboardControl: KeyboardMidiControl = KeyboardMidiControl(Some(defaultRuntime))

  /**
   * TODO: Keep track of all audio resources currently in use
   * @return returns all the available midi input devices.
   */
  def MIDIInputs: Array[MidiDeviceTransmitter] =
    val deviceInfo = MidiSystem.getMidiDeviceInfo
    val allTransmitters = deviceInfo.map(a => Try{
      val device =  MidiSystem.getMidiDevice(a)
      println(device)
      // Open the devices for our use
      if(!device.isOpen) then
        device.open()
        device.getTransmitter
      else
        // See if we are able to get an existing transmitter. Else, open a new one.
        val a = device.getTransmitters.toArray.headOption
        a.getOrElse(device.getTransmitter)
    })
    //val res = keyboardControl +: allDevices.filter(_.isInstanceOf[Receiver])
    println(allTransmitters.mkString("Array(", ", ", ")"))
    val res = keyboardControl +: allTransmitters.collect{case Success(a:MidiDeviceTransmitter) => a}
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
