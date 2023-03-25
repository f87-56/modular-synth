package SynthSoundIO
import javax.sound.midi.{MidiSystem, Receiver, Synthesizer}
import scala.util.{Success, Try}

object AudioResourceHandler {
  def MIDIInputs =
    val dedasfklj = MidiSystem.getTransmitter
    val deviceInfo = MidiSystem.getMidiDeviceInfo
    val allDevices = deviceInfo.map(a => Try(MidiSystem.getMidiDevice(a)))
    allDevices.collect{case Success(a:Receiver) => a}

}
