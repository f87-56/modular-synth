package SynthSoundIO

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import scalafx.scene.input.KeyCode

import java.security.InvalidParameterException
import javax.sound.midi.{MidiMessage, Receiver, ShortMessage}
import scala.collection.mutable.Stack
import scala.util.{Failure, Success, Try}

class KeyboardMidiControlSpec extends AnyFlatSpec with should.Matchers with PrivateMethodTester:

  val rcvr = new Receiver:
    def send(midiMessage: MidiMessage, timeStame:Long) = {}
    override def close(): Unit = ???

  val ctl = SynthSoundIO.KeyboardMidiControl(Some(rcvr))
  val makeMessage = PrivateMethod[Try[MidiMessage]](Symbol("makeMessage"))

  "makeMessage" should "construct a correct midi message for valid inputs" in {
    var msg = ctl invokePrivate makeMessage(true, KeyCode.Z)
    msg shouldBe a [Success[ShortMessage]]
    msg.foreach(a =>
        a.getMessage shouldBe ShortMessage(ShortMessage.NOTE_ON, 0, 60, 127).getMessage)

    msg = ctl invokePrivate makeMessage(false, KeyCode.Z)
    msg shouldBe a [Success[ShortMessage]]
    msg.foreach(a =>
      a.getMessage shouldBe ShortMessage(ShortMessage.NOTE_OFF, 0, 60, 127).getMessage)

  }

  it should "Return a Failure for inputs that should not result in a valid MidiMessage" in {
      var msg = ctl invokePrivate makeMessage(false, KeyCode.Y)
      msg shouldBe a [Failure[Any]]
  }

