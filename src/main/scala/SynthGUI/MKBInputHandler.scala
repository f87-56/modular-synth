package SynthGUI

import scalafx.scene.input.{KeyCode, KeyEvent}
import scala.collection.mutable

// We implement an observer pattern that noifies observers of changes in keyboard input state.
object MKBInputHandler {

  // Objects that listen
  private val listeners:mutable.Set[KeyPressListener] = mutable.Set()

  // stores the keys that are "currently" held down
  val pressedKeys:mutable.Set[KeyCode] = mutable.Set()

  def addListener(ls:KeyPressListener) = listeners += ls
  def removeListener(ls:KeyPressListener) = listeners -= ls

  def keyDown(keyCode: KeyCode) =
    if(!pressedKeys.contains(keyCode)) then
    pressedKeys += keyCode
  def keyUp(keyCode: KeyCode) =
    pressedKeys -= keyCode

  def notifyListeners():Unit = listeners.foreach(_.onInputStateDelta)
}

// Listeners that react to changes in the input state
trait KeyPressListener:
  // Called when a change in the keyboard state occurs
  def onInputStateDelta(keyCode: KeyCode):Unit