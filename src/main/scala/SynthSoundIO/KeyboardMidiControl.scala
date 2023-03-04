package SynthSoundIO

import SynthGUI.{KeyPressListener, MKBInputHandler}
import com.sun.net.httpserver.Authenticator.Failure
import scalafx.scene.input.KeyCode
import scalafx.scene.input.KeyEvent

import javax.sound.midi
import javax.sound.midi.*
import javax.sound.midi.Transmitter
import scala.util.Try

/**
 * Uses the Java sound library.
 */
class KeyboardMidiControl(receiver: Receiver) extends KeyPressListener:

  private class NotANoteException extends Throwable

  // Register as a listener on construction
  MKBInputHandler.addListener(this)

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
    msg.foreach(receiver.send(_, NoTimeStamp))
  override def onKeyUp(keyCode: KeyCode) =
    val msg = makeMessage(false, keyCode)
    msg.foreach(receiver.send(_, NoTimeStamp))

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


end KeyboardMidiControl
