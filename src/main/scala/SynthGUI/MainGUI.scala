package SynthGUI

import scalafx.application.JFXApp3
import scalafx.scene.AccessibleRole.CheckBox
import scalafx.scene.Scene
import scalafx.scene.control.{Button, CheckBox, Label}
import scalafx.scene.effect.BlendMode.Blue
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.{Pane, VBox}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.FileChooser
import scalafx.Includes._


object MainGUI extends JFXApp3:

  override def start(): Unit =

    // Creation of "primary stage", the appilcation window
    // Uses the anonymous subclass syntax. Interesting!
    stage = new JFXApp3.PrimaryStage:
      title = "Hello Stage"
      width = 600
      height = 450

    // Root GUI component gets created and added to scene
    //val root = Pane()   // Pane, a "baseplate-like?" structure?
    val root = VBox()   // Pane, a "baseplate-like?" structure?
    root.spacing = 5
    // A scene gets set on our pane
    val scene = Scene(parent = root)  // A scene contains the scene graph (the data structure that represents the UI)
    stage.scene = scene

    scene.onKeyPressed = (event) => {
      println(event.getEventType)
      MKBInputHandler.pressedKeys += event.getCode
      println(MKBInputHandler.pressedKeys.mkString(","))
    }

    scene.onKeyReleased = (event) => {println(event.getEventType)
      MKBInputHandler.pressedKeys -= event.getCode
      println(MKBInputHandler.pressedKeys.mkString(","))
    }
    scene.onKeyTyped = (event) => {println(event.getEventType)}
    scene.onInputMethodTextChanged = (event) => {println("sdafsafj")}


// Adding a text field
