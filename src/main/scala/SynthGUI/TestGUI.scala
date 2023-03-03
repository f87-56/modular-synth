import scalafx.application.JFXApp3
import scalafx.scene.AccessibleRole.CheckBox
import scalafx.scene.Scene
import scalafx.scene.control.{Button, CheckBox, Label}
import scalafx.scene.effect.BlendMode.Blue
import scalafx.scene.layout.{Pane, VBox}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.FileChooser



object TestApp extends JFXApp3:

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

    val rectangle = new Rectangle:
      x = 100
      y = 100
      width = 50
      height = 50
      fill = Color.Blue

    root.children += rectangle

    // Adding a button
    val button = Button("I'm a button!")
    button.onAction = (event) => {println("Click");
      // Make the user choose a file!
      val fileChooser = new FileChooser
      val selectedFile = fileChooser.showOpenDialog(stage)
      println(selectedFile)
    }
    root.children += button

    // Adding a Label
    val label = Label("Ahoy, there!")
    label.textFill = Color.Blue
    label.font = Font.font(36)
    // What to focus on when the label is clicked?
    label.labelFor = rectangle
    root.children += label

    // Adding a CheckBox
    val checkBox = scalafx.scene.control.CheckBox("TextBox")

    // Properties: This allows us to automatically update values upon user interaction:
    checkBox.selected <==> label.visible
    root.children += checkBox

    // Adding a text field
