package SynthGUI

import scalafx.scene.input.{KeyCode, KeyEvent}

import scala.collection.mutable
import scalafx.Includes.*
import javafx.scene.input.KeyEvent.{KEY_PRESSED, KEY_RELEASED, KEY_TYPED}

import scala.annotation.unused

// We implement an observer pattern that notifies observers of changes in keyboard input state.
// Observe, JavaFX events exhibit strange behaviour when nonstandard characters are pressed,
// as well as when the keys k and p are pressed simultaneously with others for some reason?

object MKBInputHandler {

  // Objects that listen
  private val listeners:mutable.Set[KeyPressListener] = mutable.Set()

  // stores the keys that are "currently" held down
  private val pressedKeys:mutable.Set[KeyCode] = mutable.Set()

  def addListener(ls:KeyPressListener):Unit = listeners += ls
  @unused
  def removeListener(ls:KeyPressListener):Unit = listeners -= ls

  // Handle a key event
  def keyInput(keyEvent: KeyEvent):Unit =
    keyEvent.eventType match
      case a if a == KEY_PRESSED => keyDown(keyEvent.code)
      case b if b == KEY_RELEASED => keyUp(keyEvent.code)


  /**
   * registers a key press
   * @param keyCode the key that was pressed
   * @return
   */
  def keyDown(keyCode: KeyCode):Unit =
    if !pressedKeys.contains(keyCode) then notifyKeyDown(keyCode)
    pressedKeys += keyCode

  /**
   * Releases a key
   * @param keyCode the key that was released
   */
  def keyUp(keyCode: KeyCode):Unit =
    notifyKeyUp(keyCode)
    pressedKeys -= keyCode

  private def notifyKeyDown(keyCode: KeyCode):Unit =
    listeners.foreach(_.onNewKeyDown(keyCode))
  private def notifyKeyUp(keyCode: KeyCode):Unit =
    listeners.foreach(_.onKeyUp(keyCode))
}

trait KeyPressListener:
  // Called when a change in the keyboard state occurs
  def onNewKeyDown(keyCode: KeyCode):Unit
  def onKeyUp(keyCode: KeyCode):Unit
