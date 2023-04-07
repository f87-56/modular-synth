package SynthGUI

import SynthGUI.MainGUI.stage
import javafx.scene.layout.{Background, BackgroundFill}
import scalafx.geometry.{Insets, Point2D, Pos}
import scalafx.scene.{Group, Node}
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, ScrollPane}
import scalafx.scene.input.MouseButton
import scalafx.scene.paint.Color.*
import scalafx.scene.layout.{CornerRadii, Pane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.transform.*
import scalafx.stage.FileChooser

import scala.util.Try


/**
 * Represents the "drafting table" that the synths are built in
 * Gestures adapted from https://stackoverflow.com/questions/27356577/scale-at-pivot-point-in-an-already-scaled-node
 * and https://docs.oracle.com/javafx/2/events/DraggablePanelsExample.java.htm
 *
 * Zoomable and pannable ScrollPane, adapted from
 * https://stackoverflow.com/questions/39827911/javafx-8-scaling-zooming-scrollpane-relative-to-mouse-position
 */
class GUIWorkspace extends ScrollPane:

  private val MIN_SCROLL = 0.01
  private val MAX_SCROLL = 100.0
  private val zoomIntensity = 0.04

  private var zoomScale = 1.0

  // Constructing
  val synthCanvas: Pane = new Pane()
  synthCanvas.setMinSize(100000,100000)
  synthCanvas.setBackground(new Background(new BackgroundFill(Color.Gold, CornerRadii.Empty, Insets.Empty)))

  private val zoomNode:Node = Group(synthCanvas)
  this.setContent(outerNode(zoomNode))

  this.pannable = true
  // Subject to change
  this.hbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
  this.vbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
  this.fitToWidth = true
  this.fitToHeight = true

  // Initialize things that cannot be initialized in constructor (They don't work properly)
  def initialize(): Unit =
    // Where should the camera be centered at the start? (fractional)
    val hStartPos = 0.9
    this.layout()

    //this.hvalue = this.hmax.value* hStartPos
    //this.vvalue = this.vmax.value/2

  // We wrap our canvas in this construct.
  def outerNode(node:Node):Node =
    val outrNode = centeredNode(node)
    // The lambda that gets executed on scroll.
    outrNode.setOnScroll(
      event =>
        // Preserve default scroll functionality if ctrl held.
        if(!event.isControlDown) then
          event.consume()
          onScroll(event.getTextDeltaY,            // event.getdeltay/event.getmultipliery
            Point2D(event.getX, event.getY)))
    outrNode

  // A node within a VBox that is centered in the middle
  def centeredNode(node:Node):Node =
    val vBox = new VBox(node)
    vBox.setAlignment(Pos.Center)
    vBox

  def updateScale() =
    synthCanvas.setScaleX(zoomScale)
    synthCanvas.setScaleY(zoomScale)

  synthCanvas.children += new Rectangle:
    x = 100
    y = 100
    width = 50
    height = 50
    fill = Red

  synthCanvas.children += new Rectangle:
    x = -100
    y = -100
    width = 50
    height = 50
    fill = Red

  val button = Button("I'm a button!")
  button.onAction = event => {
    println("Click")
    // Make the user choose a file!
    val fileChooser = new FileChooser
    val selectedFile = fileChooser.showOpenDialog(stage)
    println(selectedFile)
  }

  synthCanvas.children += new VBox:
    children = button
    this.translateX = 300
    this.translateY = 250

  // Reset zoom and position
  def reset() = ???

  // Zooming in/out. Called from the callback function
  private def onScroll(wheelDelta:Double, mousePoint:Point2D): Unit =
    // Assume uniform scaling
    val zoomFactor = math.exp(wheelDelta * zoomIntensity)

    val innerBounds = zoomNode.getLayoutBounds
    val vpBounds = this.getViewportBounds

    // Pixel offsets from [0,1] range
    val valX = this.getHvalue * (innerBounds.getWidth - vpBounds.getWidth)
    val valY = this.getVvalue * (innerBounds.getHeight - vpBounds.getHeight)

    zoomScale = zoomScale * zoomFactor
    updateScale()
    this.layout()     // refresh ScrollPane scroll positions and target bounds

    // convert canvas coordinates to zoomTarget coordinates
    val posInZoomTarget = synthCanvas.parentToLocal(zoomNode.parentToLocal(mousePoint))

    // calculate adjustment of scroll position in pixels
    val adjustment = synthCanvas.getLocalToParentTransform.deltaTransform(
      posInZoomTarget.multiply(zoomFactor - 1))

    // convert back to range [0,1] (range of the scrollbars's values)
    // Too large/small values are automatically corrected by ScrollPane
    val updatedInnerBounds = zoomNode.getBoundsInLocal
    this.setHvalue((valX + adjustment.getX) / (updatedInnerBounds.getWidth - vpBounds.getWidth))
    this.setVvalue((valY + adjustment.getY) / (updatedInnerBounds.getHeight - vpBounds.getHeight))

  end onScroll

end GUIWorkspace

// A class to handle our positions
case class Vec2(x:Double, y:Double)