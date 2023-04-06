package SynthGUI

import SynthSoundIO.AudioResourceHandler
import javafx.event.{Event, EventHandler}
import javafx.geometry.Insets
import scalafx.application.JFXApp3
import scalafx.scene.AccessibleRole.CheckBox
import scalafx.scene.Scene
import scalafx.scene.control.{Button, CheckBox, CheckMenuItem, Label, Menu, MenuBar, MenuItem}
import scalafx.scene.effect.BlendMode.Blue
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.{Background, BackgroundFill, BorderPane, CornerRadii, HBox, Pane, VBox}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.FileChooser
import scalafx.Includes.*
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.{HPos, Pos}

import javax.sound.midi.{MidiDevice, MidiDeviceTransmitter}


object MainGUI extends JFXApp3:

  override def start(): Unit =

    // Creation of "primary stage", the appilcation window
    // Uses the anonymous subclass syntax. Interesting!
    stage = new JFXApp3.PrimaryStage:
      title = "Modular synth"
      width = 600
      height = 450

    // Root GUI component gets created and added to scene
    //val root = Pane()   // Pane, a "baseplate-like?" structure?
    val root = new BorderPane()   // Pane, a "baseplate-like?" structure?
    //root.spacing = 5
    // A scene gets set on our pane
    val scene = Scene(parent = root)  // A scene contains the scene graph (the data structure that represents the UI)
    stage.scene = scene

    // The main synth runtime.
    val mainRuntime = AudioResourceHandler.defaultRuntime
    val keys = AudioResourceHandler.keyboardControl
    mainRuntime.openOutput()

    val topBar = new MenuBar:
      useSystemMenuBar = true
      menus = List(
        new Menu("Midi input"){
          val refreshButton: MenuItem = MenuItem("Refresh MIDI devices")

          //private var midiDevices:Set[MidiDevice] = Set()
          onShowing = _ => items = makeMidiDeviceList

          // "recalcualte" the list of available devices without changing the existing state.
          def makeMidiDeviceList: Array[MenuItem] =
            val devices =
              AudioResourceHandler.MIDIInputs
            val deviceButtons = devices.map{
              a => new CheckMenuItem(a.getMidiDevice.getDeviceInfo.toString):
                selected = {
                  println(a.getReceiver)
                  if(a.getReceiver != null) then true
                  else false
                }

                // TODO: This whole section should be done more safely in AudioResourceHandler
                // TODO: e.g. we now hog all possible MIDI transmitters when getting the list
                selected.onInvalidate {
                  // If we have checked it, set it to send to our synth.
                  if selected.value then
                    a.setReceiver(mainRuntime)
                  // If not in use in another runtime, set to null
                  // This adheres to the expected behaviour in java sound.
                  else if a.getReceiver == mainRuntime then
                    // TODO: THIS SHOULD NOT BE DONE HERE, BUT IN THE AudioResourceHandler CLASS
                    a.setReceiver(null)
                    a.close()
                }
            }
            deviceButtons :+ refreshButton

          items = makeMidiDeviceList
          refreshButton.onAction = _ => items = makeMidiDeviceList
        },
        new Menu("Settings"),
        new Menu("Help")
      )

    // The bottom bar displays log messages.
    val bottomBar = new HBox() with LogListener:
      val messageText = new Label:
        text = "Log messages appear here"
      children = messageText
      override def onNewMessage(): Unit =
        messageText.text = OutputLog.lastLog

    bottomBar.alignment = Pos.BaselineRight

    val workspace = GUIWorkspace()

    root.top = topBar
    root.bottom = bottomBar
    root.center = workspace

    scene.onKeyPressed = (event) => {
      MKBInputHandler.keyInput(event)
    }
    scene.onKeyReleased = (event) => {
      MKBInputHandler.keyInput(event)
    }
  // Adding a text field

  end start
end MainGUI


