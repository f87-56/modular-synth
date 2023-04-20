package SynthGUI

import SynthGUI.GUISynthComponent.DragContext
import SynthGUI.LineSocket.{lastDragSource, setLastDragSource}
import scalafx.scene.layout.{HBox, Pane, StackPane, VBox}
import SynthLogic.{Parameter, SynthComponent}
import javafx.scene.control.CheckBox
import javafx.util.StringConverter
import javafx.util.converter.IntegerStringConverter
import scalafx.event.EventHandler
import scalafx.scene.input.{KeyCode, MouseEvent, TransferMode}
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.{Insets, Point2D, Pos}
import scalafx.scene.control.{Label, Spinner, SpinnerValueFactory, TextField, TextFormatter}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.{Circle, Line, Rectangle}
import scalafx.Includes.*

import java.net.Socket
import scala.collection.mutable
import scala.util.{Failure, Try}

class GUISynthComponent[T](val canvas:SynthCanvas, val synthComponent:SynthComponent[_]) extends VBox:

  val parameters = synthComponent.parameters.map(a => GUISynthParameter[Any](canvas, this, a))

  val outputSocket =
    // the output socket
    new LineSocket(canvas, GUISynthComponent.this):
      alignment = Pos.BaselineRight

  this.children +=
    new Label():
      text = "Name of node"
      padding = Insets(5)
  this.children +=
    new HBox():
      children +=
        new Label():
          text = s"out (${classOf[Int].toString})"
          padding = Insets(5)
      children += outputSocket
      this.alignment = Pos.BaselineRight

    parameters.foreach(this.children += _)

  this.layout()

  this.style =
    """
      |-fx-background-color: #f4f4f4;
      |    -fx-border-color: black;
      |    -fx-border-width: 1px;
      |    -fx-border-radius: 5px;
      |    -fx-background-radius: 5px;
      |    -fx-effect: dropshadow(three-pass-box, rgba(0,0,0,0.5), 10, 0, 0, 0);
      |""".stripMargin

  // Check if the stage this is in has been drawn
  private def isSceneDrawn:Boolean =
    val scene = Option(this.getScene)
    val window = scene.map(_.getWindow)
    val showing = window.map(_.isShowing)
    scene.isDefined && window.isDefined && showing.forall(_ == true)

  // Prevent from being created outside bounds
  // Don't do this if we have yet to draw anything
  if (isSceneDrawn) then
    canvas.restrictToBounds(this)
  // And from going out of bounds

  this.width.onInvalidate(
    if(isSceneDrawn) then
      canvas.restrictToBounds(this)
  )
  this.height.onInvalidate(
    if(isSceneDrawn) then
      canvas.restrictToBounds(this)
  )
  this.translateX.onInvalidate(
    if (isSceneDrawn) then
      canvas.restrictToBounds(this)
  )
  this.translateY.onInvalidate(
    if (isSceneDrawn) then
      canvas.restrictToBounds(this)
  )

  // Make the node draggable. From https://docs.oracle.com/javafx/2/events/filters.htm#BCFFCAIH
  this.onMousePressed = event =>
    val posInParent = localToParent(event.getX, event.getY)
    DragContext.mouseAnchorX = posInParent.x
    DragContext.mouseAnchorY = posInParent.y
    DragContext.initialTranslateX = this.getTranslateX
    DragContext.initialTranslateY = this.getTranslateY
    event.consume()

  // Nodes can be dragged off-screen
  this.onMouseDragged = event =>
    val posInParent = localToParent(event.getX, event.getY)
    this.translateX = DragContext.initialTranslateX + posInParent.x - DragContext.mouseAnchorX
    this.translateY = DragContext.initialTranslateY + posInParent.y - DragContext.mouseAnchorY

    canvas.restrictToBounds(this)
    event.consume()

  // Get keyboard focus
  this.onMouseClicked = event =>
    this.requestFocus()
    event.consume()

  this.onKeyPressed = event =>
    if(event.code == KeyCode.Delete)
      this.delete()
      event.consume()

  // TODO: handle synth logic here
  private def delete() =
    // Synth logic side
    canvas.synth.removeComponent(this.synthComponent)
    // GUI side
    parameters.foreach(_.disconnect())
    outputSocket.removeConnections()
    canvas.requestFocus()
    Try(canvas.children.remove(this))

end GUISynthComponent
object GUISynthComponent:
  // Store information of ongoing drag
  private case object DragContext {
    var mouseAnchorX: Double = 0.0
    var mouseAnchorY: Double = 0.0
    var initialTranslateX: Double = 0.0
    var initialTranslateY: Double = 0.0
  }
end GUISynthComponent

class GUISynthParameter[T](val canvas:SynthCanvas,
                           val parentComponent:GUISynthComponent[_],
                           val parameter: Parameter[_]) extends HBox:
  this.padding = Insets(20)
  spacing = 5
  private val inputSocket = new LineSocket(canvas, this):
    alignment = Pos.BaselineLeft
  if(parameter.takesInput) then
    this.children += inputSocket
  // Populate this node depending on the data type
  parameter.defaultValue match
    case a:Int =>
      // For some reason, automatic input filtering works on Double spinners, but not integer ones.
      // Man I hate scalaFX...
      this.children += new Spinner[Int](Int.MinValue, Int.MaxValue, a, 1):
        this.editor.value.textProperty().onChange {(src, oldValue, newValue) =>
          // Set to 0 if no numbers present
          println(newValue)
          if """\d+""".r.findFirstIn(newValue).isEmpty then
            editor.value.text = "0"
          else if(newValue.toIntOption.isEmpty) then
            println("AAAAARGGGH")
            editor.value.text = oldValue}
        this.value.onChange{(src, oldVal, newVal) =>
          parameter.defaultValue = newVal
        }
        editable = true
    case b:Double =>
      this.children += new Spinner[Double](Double.MinValue, Double.MaxValue, b, 0.01):
        editable = true
        this.value.onChange { (src, oldVal, newVal) =>
          parameter.defaultValue = newVal
        }
    case c:Boolean =>
      this.children += new CheckBox():
        this.selected.onChange { (src, oldVal, newVal) =>
          parameter.defaultValue = newVal
        }
    case d:String =>
      this.children += new TextField()
  this.children += new Label:
    text = "Hello!"

  style =
    """
      |-fx-background-color: #f4f4f4;
      |    -fx-border-color: black;
      |    -fx-border-width: 1px;
      |    -fx-border-radius: 5px;
      |    -fx-background-radius: 5px;
      |    -fx-padding: 5px;
      |""".stripMargin

  def disconnect(): Unit =
    this.inputSocket.removeConnections()

end GUISynthParameter

/**
 * @param isOutput Is this an output or an input socket?
 */
class LineSocket(val canvas: SynthCanvas, val parentNode:GUISynthParameter[_]|GUISynthComponent[_]) extends StackPane:

  private val (isOutput: Boolean,
  parentGUISynthComponent:GUISynthComponent[?],
  parentGUISynthParam:Option[GUISynthParameter[?]]) =
    parentNode match
      case a: GUISynthComponent[_] => (true, a, None)
      case b: GUISynthParameter[_] => (false, b.parentComponent, Some(b))

  // The style of line used to connect these sockets
  private val connections:scala.collection.mutable.Buffer[ConnectorLine] = mutable.Buffer()
  def addConnection(c:ConnectorLine):Unit =
    if(isOutput || connections.isEmpty) then
      connections += c
    // Illegal operation, we cannot add more.
    else
      removeConnections()
      connections += c

  private val SocketSize = 3.0
  private val plugSocket = Circle(SocketSize)
  plugSocket.stroke = Color.Black
  plugSocket.fill = Color.White
  plugSocket.mouseTransparent = true
  plugSocket.alignmentInParent = Pos.Center

  style =
    """
      |-fx-background-color: #f4f4f4;
      |    -fx-background-radius: 5px;
      |    -fx-padding: 5px;
      |""".stripMargin

  // center position of our socket in the canvas
  def circPosInCanvas: Point2D = canvas.sceneToLocal(plugSocket.localToScene(plugSocket.getCenterY, plugSocket.getCenterY))

  this.children = plugSocket
  def plug() =
    plugSocket.fill = Color.Black
  def unplug() =
    plugSocket.fill = Color.White

  this.onDragDetected = event =>
    setLastDragSource(this,this)
    this.startFullDrag()
    event.consume()

  this.onMouseDragReleased = event =>
    // check if the event source is the correct one.
    // Connection must be between input and output.
    // Sockets from same node should not be connected.

    lastDragSource match
      case Some(a)
        if (a._2 eq event.getGestureSource) && (a._1.isOutput != this.isOutput) &&
          this.parentGUISynthComponent != a._1.parentGUISynthComponent =>
          // Get ordering of start and end right.
          if(this.isOutput) then
            ConnectorLine(a._1, this, canvas)
            // Make the logical connection
            a._1.parentGUISynthParam.foreach(
              _.parameter <== this.parentGUISynthComponent.synthComponent)
          else
            ConnectorLine(this, a._1, canvas)
            // Make the logical connection
            this.parentGUISynthParam.foreach{
              _.parameter <== a._1.parentGUISynthComponent.synthComponent}
      case _ => ()

  // Disconnect node
  this.onMousePressed = event =>
    if(event.isControlDown) then
      removeConnections()

  // We don't want to drag the component node if we have pressed on the socket
  this.onMouseDragged = event => event.consume()

  // TODO: Interaction with synth logic
  def disconnect(connectorLine:ConnectorLine):Unit =
    this.connections -= connectorLine
  def removeConnections():Unit=
    // SynthoLogic side
    this.parentGUISynthParam.foreach(_.parameter.x())
    if(this.isOutput) then
      this.parentGUISynthComponent.synthComponent.xAll()
    // GUI side
    this.connections.toVector.foreach(_.delete())

end LineSocket
// House static variables
object LineSocket:
  // A bit of a hack to validate the socket when connecting. Keep track of the java type version
  // of the object for identity comparison, and of the original type to access its members.
  def setLastDragSource(scalaVer:LineSocket, javaVer:javafx.scene.layout.StackPane): Unit =
    lastDragSource = Some(scalaVer, javaVer)
  private var lastDragSource:Option[(LineSocket, javafx.scene.layout.StackPane)] = None
end LineSocket


class ConnectorLine(start:LineSocket, end:LineSocket, canvas:SynthCanvas) extends Line:

  // Add to parent
  canvas.children += this
  // Possibly overwrite old connections
  start.addConnection(this)
  end.addConnection(this)

  this.stroke = Color.Black
  this.strokeWidth = 5.0
  this.mouseTransparent = true

  updatePos()
  private def updatePos(): Unit =
    this.startX = start.circPosInCanvas.x
    this.startY = start.circPosInCanvas.y
    this.endX = end.circPosInCanvas.x
    this.endY = end.circPosInCanvas.y

  start.localToSceneTransformProperty.onChange{
    updatePos()
  }
  end.localToSceneTransformProperty.onChange {
    updatePos()
  }
  def delete(): Try[Any] =
    Try{
      this.getParent match
        // We need to cast it to a type where you can remove the child.
        case a:javafx.scene.layout.Pane =>
          a.getChildren.remove(this)
        case _ => throw Exception()
      // remove this from the parents' lists
      this.start.disconnect(this)
      this.end.disconnect(this)
    }

end ConnectorLine




