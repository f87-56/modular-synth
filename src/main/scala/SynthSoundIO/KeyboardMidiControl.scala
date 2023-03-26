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
class KeyboardMidiControl(receiver: Option[Receiver]) extends MidiDeviceTransmitter with KeyPressListener:

  private class NotANoteException extends Throwable

  // Register as a listener on construction
  MKBInputHandler.addListener(this)

  private var connectedReceiver:Option[Receiver] = receiver

  // In accordance with the specification https://docs.oracle.com/javase/8/docs/api/javax/sound/midi/Transmitter.html
  override def getReceiver: Receiver =
    connectedReceiver match
      case Some(a) => a
      case _ => null
  override def setReceiver(receiver: Receiver): Unit =
    connectedReceiver = Some(receiver)

  override def getMidiDevice: MidiDevice = emptyMidiDevice
  // Always present when our application runs.
  override def close(): Unit = ()

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

  override def onNewKeyDown(keyCode: KeyCode): Unit =
    val msg = makeMessage(true, keyCode)
    msg.foreach(a => receiver.foreach(_.send(a, NoTimeStamp)))
  override def onKeyUp(keyCode: KeyCode): Unit =
    val msg = makeMessage(false, keyCode)
    msg.foreach(a => receiver.foreach(_.send(a, NoTimeStamp)))

  /**
   *
   * @param on Is this a MIDI_ON or a MIDI_OFF message?
   * @return
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


  private object emptyMidiDevice extends MidiDevice():
    override def open(): Unit = ()
    override def close(): Unit = ()
    override def isOpen: Boolean = true
    private object emptyInfo extends MidiDevice.Info("Computer keyboard","","","")
    override def getDeviceInfo: MidiDevice.Info = emptyInfo
    override def getTransmitters: util.List[Transmitter] = java.util.List.of[Transmitter]()
    override def getReceivers: util.List[Receiver] = java.util.List.of[Receiver]()
    override def getTransmitter: Transmitter = throw MidiUnavailableException()
    override def getReceiver: Receiver = throw MidiUnavailableException()
    override def getMaxTransmitters: Int = 0
    override def getMaxReceivers: Int = 0
    override def getMicrosecondPosition: Long = 0L
  end emptyMidiDevice

end KeyboardMidiControl

