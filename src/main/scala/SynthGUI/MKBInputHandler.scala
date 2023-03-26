package SynthGUI

import scalafx.scene.input.{KeyCode, KeyEvent}
import scala.collection.mutable
import scalafx.Includes.*
import javafx.scene.input.KeyEvent.{KEY_PRESSED, KEY_RELEASED, KEY_TYPED}

// We implement an observer pattern that noifies observers of changes in keyboard input state.
// Observe, JavaFX events exhibit strange behaviour when nonstandard characters are pressed,
// as well as when the keys k and p are pressed simultaniously with others for some reason?

object MKBInputHandler {

  // Objects that listen
  private val listeners:mutable.Set[KeyPressListener] = mutable.Set()

  // stores the keys that are "currently" held down
  val pressedKeys:mutable.Set[KeyCode] = mutable.Set()

  def addListener(ls:KeyPressListener) = listeners += ls
  def removeListener(ls:KeyPressListener) = listeners -= ls

  def keyInput(keyEvent: KeyEvent) =
    keyEvent.eventType match
      case a if a == KEY_PRESSED => keyDown(keyEvent.code)
      case b if b == KEY_RELEASED => keyUp(keyEvent.code)


  def keyDown(keyCode: KeyCode) =
    if(!pressedKeys.contains(keyCode)) then notifyKeyDown(keyCode)
    pressedKeys += keyCode
  def keyUp(keyCode: KeyCode) =
    notifyKeyUp(keyCode)
    pressedKeys -= keyCode

  private def notifyKeyDown(keyCode: KeyCode):Unit =
    listeners.foreach(_.onNewKeyDown(keyCode))
  private def notifyKeyUp(keyCode: KeyCode):Unit =
    listeners.foreach(_.onKeyUp(keyCode))
}


/**
 *   override def onNewKeyDown = ???
 */
trait KeyPressListener:
  // Called when a change in the keyboard state occurs
  def onNewKeyDown(keyCode: KeyCode):Unit
  def onKeyUp(keyCode: KeyCode):Unit
