package SynthSoundIO
import SynthGUI.OutputLog

import javax.sound.midi.{MidiDevice, MidiDeviceReceiver, MidiDeviceTransmitter, MidiSystem, Receiver, Synthesizer, Transmitter}
import scala.util.{Success, Try}
import scala.jdk.CollectionConverters.*
object AudioResourceHandler {
  
  // The canonical runtime
  val defaultRuntime: SynthRuntime = SynthRuntime()
  // The "canonical instance of the keyboard control.
  val keyboardControl: KeyboardMidiControl = KeyboardMidiControl(Some(defaultRuntime))

  // Keeps track of resources we use
  private var openedDevices:List[MidiDevice] = List()

  /**
   * @return returns all the available midi input devices.
   */
  def MIDIInputs: Array[MidiDevice] =
    val deviceInfo = MidiSystem.getMidiDeviceInfo
    val validDevices =  deviceInfo.flatMap(a => Try {
      MidiSystem.getMidiDevice(a)
    }.toOption).filter(_.getMaxTransmitters != 0)
    keyboardControl.getMidiDevice +: validDevices

  /**
   * Sets the synth runtime this device sends to.
   * @param transmitter
   * @param synthRuntime
   * @return
   */
  def connectToSynth(midiDevice: MidiDevice, synthRuntime: SynthRuntime): Try[Unit] =
    println("OPENING: " + midiDevice)
    Try{midiDevice.open()
      midiDevice.getTransmitter.setReceiver(synthRuntime)
      openedDevices = (openedDevices :+ midiDevice).distinct
    }

  /**
   * Free this transmitter for the use of other applications.
   * Adheres to the expected behaviour in java sound.
   * @param midiDevice
   * @param synthRuntime
   * @return
   */
  def freeDevice(midiDevice: MidiDevice): Try[Unit] =
    Try{
      //transmitter.getMidiDevice.close()
      println("FREEING: " + midiDevice)
      midiDevice.getTransmitters.asScala.foreach(a => a.close())
      midiDevice.close()
      openedDevices = openedDevices.distinct.filter(_ != midiDevice)
    }

  def freeAllDevices(): Try[Unit] =
    Try{
      MIDIInputs.foreach(
        freeDevice)
      openedDevices.foreach(
        freeDevice
      )
      //mi.foreach(freeTransmitter)
    }

}
