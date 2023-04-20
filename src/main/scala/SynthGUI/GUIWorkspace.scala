package SynthGUI

import SynthGUI.MainGUI.stage
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Point2D, Pos}
import scalafx.scene.{Group, Node}
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, ComboBox, ScrollPane}
import scalafx.scene.input.{KeyCode, KeyEvent, MouseButton}
import scalafx.scene.paint.Color.*
import scalafx.scene.layout.{CornerRadii, Pane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.transform.*
import scalafx.stage.FileChooser
import scalafx.Includes.*
import scalafx.application.Platform

import java.awt.{MouseInfo, Point}
import scala.concurrent.Future
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

  private val MinZoom = 0.1
  private val MaxZoom = 4.0
  private val zoomIntensity = 0.04

  private var zoomScale = 1.0

  // Constructing
  val synthCanvas = SynthCanvas()
  //synthCanvas.setBackground(new Background(new BackgroundFill(Color.Gold, CornerRadii.Empty, Insets.Empty)))

  private val zoomNode:Node = Group(synthCanvas)
  this.setContent(outerNode(zoomNode))

  this.pannable = true
  // Subject to change
  this.hbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
  this.vbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
  this.fitToWidth = true
  this.fitToHeight = true



  // Set initial scroll bar positions
  private val hStartPos = 0.9
  //this.hvalue = this.hmax.value * hStartPos
  //this.vvalue = this.vmax.value / 2
  this.layout()

  // We wrap our canvas in this construct.
  def outerNode(node:Node):Node =
    val outrNode = centeredNode(node)
    // Request keyboard focus on left click
    outrNode.onMouseClicked = event =>
      outrNode.requestFocus()
    // The lambda that gets executed on scroll.
    outrNode.setOnScroll(
      event =>
        // Preserve default scroll functionality if ctrl held.
        if(!event.isControlDown) then
          event.consume()
          onScroll(event.getTextDeltaY,            // event.getdeltay/event.getmultipliery
            Point2D(event.getX, event.getY)))
    //Adding new components
    outrNode.onKeyPressed = event =>
      if event.code == KeyCode.Space && this.hover.value then
        event.consume()
        val a = showFinder()
        a.requestFocus()
        a.layout()
    outrNode

  // A node within a VBox that is centered in the middle
  def centeredNode(node:Node):Node =
    val vBox = new VBox(node)
    vBox.setAlignment(Pos.Center)
    vBox

  def updateScale() =
    synthCanvas.setScaleX(zoomScale)
    synthCanvas.setScaleY(zoomScale)

  this.layout()

  synthCanvas.children += new GUISynthComponent[Int](synthCanvas):
    translateX = 400
    translateY = 400

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

    val newScale = zoomScale * zoomFactor
    val newScaleClamped = if(newScale > MaxZoom) then MaxZoom
      else if(newScale < MinZoom) then MinZoom
      else newScale

    if(newScaleClamped != zoomScale) then
      zoomScale = newScaleClamped
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

  //this.onContextMenuRequested = event => ()

  private def showFinder()=
    val a = new ComponentSearchBox(synthCanvas):
      private val pos = synthCanvas.localMousePos
      translateX = pos._1
      translateY = pos._2
    a.show()
    a.editor.value.layout()
    a.editor.value.requestFocus()
    a



end GUIWorkspace

class ComponentSearchBox(val parentCanvas:SynthCanvas) extends ComboBox[String]:
  val testList:ObservableBuffer[String] = ObservableBuffer("Item0","Item1", "Humphrey Davey", "Weezer","Weezer1","Item2","Item3")
  this.items = testList
  this.editable = true

  private var oldTextVal = ""
  this.show()
  this.editor.value.requestFocus()

  parentCanvas.children += this

  private var valueChangedFlag = false

  this.editor.value.textProperty.onChange{(source, oldValue, newValue) =>
    // Reset value if it corresponds to nothing
    // Prevent from refiltering if we are scrolling down the list
    if(!(oldValue.trim == newValue.trim) && !valueChangedFlag) then
      this.getSelectionModel.clearSelection()
      this.value.value = newValue
      val newStr = editor.value.textProperty.getValue.trim.toLowerCase
      this.items = testList.filter(_.toLowerCase.contains(newStr))
    this.show()
    valueChangedFlag = false
  }

  // Literally nothing's changed.
  // The selected value
  this.value.onChange { (source, oldValue, newValue) =>
    valueChangedFlag = true
  }

  // User has canceled search
  this.focused.onChange{ (op, oldVal, newVal) =>
    if !newVal then
      this.delete()
  }

  // User has made descision.
  // Observe: Even esc key will provoke this.
  this.onHidden = (event) =>
    if (testList.contains(this.value.value)) then
      val pos = (this.getTranslateX, this.getTranslateY)
      val a = new GUISynthComponent[Int](parentCanvas):
        translateX = pos._1
        translateY = pos._2
      this.parentCanvas.children += a
      a.layout()
    this.delete()

  private def delete() =
    parentCanvas.requestFocus()
    Try(parentCanvas.children.remove(this))

  /*// Handle the ESC key separately
  this.editor.value.onKeyPressed = (event) =>
    if(event.code == KeyCode.Escape) then
      this.value.value = ""
      this.editor.value.textProperty.value = ""*/

end ComponentSearchBox
