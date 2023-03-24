package SynthGUI

import javafx.geometry.Insets
import scalafx.application.JFXApp3
import scalafx.scene.AccessibleRole.CheckBox
import scalafx.scene.Scene
import scalafx.scene.control.{Button, CheckBox, Label, Menu, MenuBar}
import scalafx.scene.effect.BlendMode.Blue
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.{Background, BackgroundFill, BorderPane, CornerRadii, HBox, Pane, VBox}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.FileChooser
import scalafx.Includes.*
import scalafx.geometry.{HPos, Pos}


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
    val root = new BorderPane()   // Pane, a "baseplate-like?" structure?
    //root.spacing = 5
    // A scene gets set on our pane
    val scene = Scene(parent = root)  // A scene contains the scene graph (the data structure that represents the UI)
    stage.scene = scene

    val topBar = new MenuBar:
      useSystemMenuBar = true
      menus = List(
      new Menu("Midi input"),
      new Menu("Settings"),
      new Menu("Help")
        )

    val bottomBar = new HBox() with LogListener:
      val messageText = new Label:
          text = "Log messages appear here"
      children = messageText

      override def onNewMessage: Unit =
        messageText.text = OutputLog.lastLog

    bottomBar.alignment = Pos.BaselineRight

    root.top = topBar
    root.bottom = bottomBar

    // The main synth runtime.
    val runtime = SynthSoundIO.SynthRuntime()

    scene.onKeyPressed = (event) => {
      MKBInputHandler.keyInput(event)
    }
    scene.onKeyReleased = (event) => {
      MKBInputHandler.keyInput(event)
    }


// Adding a text field
