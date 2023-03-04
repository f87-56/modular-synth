package SynthSoundIO

import scalafx.scene.input.KeyEvent

import javax.sound.midi
import javax.sound.midi.*
import javax.sound.midi.Transmitter

/**
 * Uses the Java sound library.
 */
class KeyboardMidiControl:

  val KeyNoteMap:Map[String, Int] =
    Map("z" -> 60,
      "s" -> 61,
      "x" -> 62,
      "d" -> 63,
      "c" -> 64,
      "f" -> 65,
      "v" -> 66,
      "g" -> 67,
      "b" -> 68,
      "h" -> 69,
      "n" -> 70,
      "j" -> 71,
      "m" -> 72,
      "k" -> 73,
    )

/*
  def makeMessage(keyEvent: KeyEvent):Try[ShortMessage] =
    keyEvent.getEventType match
      case
    val msg:ShortMessage = ShortMessage()
    msg.setMessage(
*/