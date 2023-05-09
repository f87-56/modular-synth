package SynthGUI

import SynthFileIO.SynthSerializer
import SynthSoundIO.AudioResourceHandler
import javafx.application.Platform
import javafx.event.{Event, EventHandler}
import javafx.geometry.Insets
import scalafx.application.JFXApp3
import scalafx.scene.AccessibleRole.CheckBox
import scalafx.scene.Scene
import scalafx.scene.control.{Alert, Button, ButtonType, CheckBox, CheckMenuItem, Label, Menu, MenuBar, MenuItem}
import scalafx.scene.effect.BlendMode.Blue
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.{Background, BackgroundFill, BorderPane, CornerRadii, HBox, Pane, VBox}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.FileChooser
import scalafx.Includes.*
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.{HPos, Pos}
import io.circe.syntax.*
import scalafx.scene.control.Alert.AlertType

import java.io.File
import javax.sound.midi.{MidiDevice, MidiDeviceTransmitter}
import scala.util.{Failure, Success, Try}


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
    mainRuntime.openOutput()

    // The "workbench" where synths are built
    val workspace = GUIWorkspace(mainRuntime.activeSynth)

    // The last directory the user has saved to
    var currentSynthDir:Option[File] = None

    // Loads and sets up a synth
    def synthLoadSetup(toBeLoaded: File|String): Unit =
      val a = {toBeLoaded match
        case f:File =>
          currentSynthDir = Some(f)
          SynthSerializer.loadCanvas(f)
        case s:String =>
          SynthSerializer.loadCanvas(s)}

      a match
        case Success(value) =>
          workspace.replaceCanvas(value)
          value.requestFocus()
          mainRuntime.activeSynth = value.synth
        case Failure(exception) =>
          OutputLog.log("Could not load file: " + exception.toString)
          System.err.println("Could not load file: " + exception.toString)
    end synthLoadSetup

    // Setup a workspace ready for editing
    synthLoadSetup(SynthSerializer.EmptyWorkspace)

    // Ask the user if they want to save their current synth
    def savePrompt(): Unit =
      val yButton = new ButtonType("Yes")
      val nButton = new ButtonType("No")

      val alert: Alert = new Alert(AlertType.Information):
        title = "Save current synth?"
        headerText = "Do you want to save the current synth?"

        this.buttonTypes = List(yButton, nButton)
      val result: Option[ButtonType] = alert.showAndWait()
      result.foreach(a =>
        if a == yButton then
          if (currentSynthDir.isDefined) then
            currentSynthDir.foreach(saveSynth)
          else
            saveFile.foreach(saveSynth)
      )

    def saveSynth(path:File): Unit =
      currentSynthDir = Some(path)
      SynthSerializer.saveCanvas(workspace.synthCanvas, path)
      OutputLog.log("Saved synth to: " + path)

    // TODO: Add some logging here
    def saveFile: Option[File] =
      val fileChooser = new FileChooser:
        currentSynthDir.foreach(a => initialDirectory = File(a.getParent))
        title = "Where to save the current synth?"
      val sFile = Option(fileChooser.showSaveDialog(stage))
      sFile

    // TODO: Add some logging here
    def getFile: Option[File] =
      val fileChoose = new FileChooser:
        currentSynthDir.foreach(a => initialDirectory = File(a.getParent))
        title = "Select synth to load"
      val selectedFile = Option(fileChoose.showOpenDialog(stage))
      selectedFile

    // The top bar of the app
    val topBar = new MenuBar:
      useSystemMenuBar = true

      menus = List(
        // Files, opeining and saving
        new Menu("File"){
          items += new MenuItem("Load synth"):
            this.onAction = * => {
              savePrompt()
              getFile.foreach(a =>
                synthLoadSetup(a))
              }
          items += new MenuItem("Save synth"):
            this.onAction = * => {
              saveFile.foreach(a => saveSynth(a))
            }
          items += new MenuItem("Create empty"):
            this.onAction = * => {
              synthLoadSetup(SynthSerializer.EmptyWorkspace)
              // We don't want to accidentally overwrite the default.
              currentSynthDir = None
            }
        },
        new Menu("Midi input"){
          val refreshButton: MenuItem = MenuItem("Refresh MIDI devices")

          onShowing = _ => items = makeMidiDeviceList

          // "recalculate" the list of available devices without changing the existing state.
          def makeMidiDeviceList: Array[MenuItem] =
            val devices =
              AudioResourceHandler.MIDIInputs
            val deviceButtons = devices.map{
              a => new CheckMenuItem(a.getMidiDevice.getDeviceInfo.toString):
                selected = {
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
    val bottomBar: HBox with LogListener = new HBox() with LogListener:
      this.setBackground(new Background(Array(new BackgroundFill(Color.Gray, CornerRadii.Empty, Insets.EMPTY))))
      val messageText: Label = new Label:
        this.prefHeight = 10
        text = "Log messages appear here"
        this.text.onChange{(src, oldVal, newVal) =>
          this.text = newVal.split('\n').mkString(" ")
          println(newVal.split('\n').mkString(" "))
        }
      children = messageText
      override def onNewMessage(): Unit =
        messageText.text = OutputLog.lastLog

    bottomBar.alignment = Pos.BaselineRight

    root.center = workspace
    root.top = topBar
    root.bottom = bottomBar

    scene.onKeyPressed = event => {
      // The save feature
      if(event.code == KeyCode.S && event.isControlDown) then
        event.consume()
        currentSynthDir match
          case Some(a) => saveSynth(a)
          case None => saveFile.foreach(saveSynth)
        currentSynthDir.foreach(saveSynth)
      else if (event.code == KeyCode.P) then
        println(workspace.synthCanvas.synth)
        
      // Load default synth, a debugging shortcut
      /*
      else if (event.code == KeyCode.L && event.isControlDown) then
        // TODO: Replace with save function
        event.consume()
        // TODO: Fix this nonsense also
        savePrompt
        synthLoadSetup(File("./DefaultSynth"))*/

      else
        MKBInputHandler.keyInput(event)
    }
    scene.onKeyReleased = event => {
      MKBInputHandler.keyInput(event)
    }

  end start



end MainGUI


