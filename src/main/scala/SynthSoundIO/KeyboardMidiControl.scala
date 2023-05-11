package SynthSoundIO

import SynthGUI.{KeyPressListener, MKBInputHandler}
import com.sun.net.httpserver.Authenticator.Failure
import scalafx.scene.input.KeyCode
import scalafx.scene.input.KeyEvent

import java.util
import javax.sound.midi
import javax.sound.midi.*
import javax.sound.midi.Transmitter
import scala.util.Try

/**
 * Uses the Java sound library.
 */
class KeyboardMidiControl(private var receiver: Option[Receiver]) extends MidiDeviceTransmitter with KeyPressListener:

  private class NotANoteException extends Throwable

  // Register as a listener on construction
  MKBInputHandler.addListener(this)


  // In accordance with the specification https://docs.oracle.com/javase/8/docs/api/javax/sound/midi/Transmitter.html
  // In java sound's implementations, getReceiver should be expected to return null.
  override def getReceiver: Receiver =
    receiver match
      case Some(a) => a
      case _ => null
  override def setReceiver(newReceiver: Receiver): Unit =
    println("New receiver: " + newReceiver)
    receiver = Option(newReceiver)

  private val parentDevice = EmptyMidiDevice(this)
  override def getMidiDevice: MidiDevice = parentDevice
  // Always present when our application runs.
  override def close(): Unit = this.receiver = None

  private val KeyNoteMap:Map[KeyCode, Int] =
    Map(KeyCode.Z -> 60,
      KeyCode.S -> 61,
      KeyCode.X -> 62,
      KeyCode.D -> 63,
      KeyCode.C -> 64,
      KeyCode.F -> 65,
      KeyCode.V -> 66,
      KeyCode.G -> 67,
      KeyCode.B -> 68,
      KeyCode.H -> 69,
      KeyCode.N -> 70,
      KeyCode.J -> 71,
      KeyCode.M -> 72,
      KeyCode.K -> 73,
    )

  private val NoTimeStamp = -1L
  private val DefaultVelocity = 127
  private val DefaultChannel = 0


  /**
   * Key that was not pressed before has been pressed
   * @param keyCode The code of the key
   */
  override def onNewKeyDown(keyCode: KeyCode): Unit =
    val msg = makeMessage(true, keyCode)
    msg.foreach(a => receiver.foreach(_.send(a, NoTimeStamp)))

  /**
   * Key that was pressed has been released
   * @param keyCode The code of the key
   */
  override def onKeyUp(keyCode: KeyCode): Unit =
    val msg = makeMessage(false, keyCode)
    msg.foreach(a => receiver.foreach(_.send(a, NoTimeStamp)))

  /**
   *
   * @param on Is this a MIDI_ON or a MIDI_OFF message?
   * @return The wrapped message if successful, Failure if not.
   */
  private def makeMessage(on:Boolean, keyCode: KeyCode):Try[ShortMessage] =
    val note = KeyNoteMap.get(keyCode)
    val command = if on then ShortMessage.NOTE_ON else ShortMessage.NOTE_OFF
    val msg:Try[ShortMessage] = Try{
      val a = ShortMessage()
      note match
        case Some(b) =>
          a.setMessage(command, DefaultChannel, b, DefaultVelocity)
          a
        case None =>
          throw NotANoteException() // In case our key did not match a note
    }
    msg
  end makeMessage


  // An empty midi device to serve as the frame for our keyboard
  private class EmptyMidiDevice(transmitter:MidiDeviceTransmitter) extends MidiDevice():
    override def open(): Unit =
      _open = true
    override def close(): Unit = _open = false
    override def isOpen: Boolean = _open

    private var _open = true
    private object emptyInfo extends MidiDevice.Info("Computer keyboard","","","")
    override def getDeviceInfo: MidiDevice.Info = emptyInfo
    override def getTransmitters: util.List[Transmitter] =
      if _open then java.util.List.of[Transmitter](transmitter)
      else java.util.List.of[Transmitter]()
    override def getReceivers: util.List[Receiver] = java.util.List.of[Receiver]()
    override def getTransmitter: Transmitter =
      if isOpen then transmitter else throw MidiUnavailableException()
    override def getReceiver: Receiver = throw MidiUnavailableException()
    override def getMaxTransmitters: Int = 1
    override def getMaxReceivers: Int = 0
    override def getMicrosecondPosition: Long = 0L
  end EmptyMidiDevice

end KeyboardMidiControl

