package SynthGUI

import SynthGUI.GUISynthComponent.DragContext
import SynthGUI.LineSocket.{lastDragSource, setLastDragSource}
import scalafx.scene.layout.{HBox, Pane, StackPane, VBox}
import SynthLogic.SynthComponent
import scalafx.event.EventHandler
import scalafx.scene.input.{KeyCode, MouseEvent, TransferMode}
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.{Insets, Point2D, Pos}
import scalafx.scene.control.Label
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.{Circle, Line, Rectangle}
import scalafx.Includes.*

import java.net.Socket
import scala.collection.mutable
import scala.util.{Failure, Try}

class GUISynthComponent[T](val canvas:SynthCanvas) extends VBox:

  val parameters = Vector(GUISynthParameter[Int](canvas, this))

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

    parameters.foreach(this.children += _)

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
                           val parentComponent:GUISynthComponent[_]) extends HBox:
  this.padding = Insets(20)
  spacing = 5
  private val inputSocket = new LineSocket(canvas, this):
    alignment = Pos.BaselineLeft
  this.children += inputSocket
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

  val (isOutput: Boolean, parentGUISynthComponent:GUISynthComponent[_]) = parentNode match
    case a: GUISynthComponent[_] => (true, a)
    case b: GUISynthParameter[_] => (false, b.parentComponent)

  // The style of line used to connect these sockets
  private val connections:scala.collection.mutable.Buffer[ConnectorLine] = mutable.Buffer()
  def addConnection(c:ConnectorLine):Unit =
    println(connections.mkString(""))
    if(isOutput || connections.isEmpty) then
      connections += c
    // Illegal operation, we cannot add more.
    else
      throw Exception()

  private val SocketSize = 3.0
  private val plugSocket = Circle(SocketSize)
  plugSocket.stroke = Color.Black
  plugSocket.fill = Color.White
  plugSocket.mouseTransparent = true

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
    // Socekts from same node should not be connnected.

    lastDragSource match
      case Some(a)
        if (a._2 eq event.getGestureSource) && (a._1.isOutput != this.isOutput) =>
          // Get ordering of start and end right.
          if(this.isOutput) then
            ConnectorLine(a._1, this, canvas)
          else
            ConnectorLine(this, a._1, canvas)
      case _ => ()

  // Disconnect node
  this.onMousePressed = event =>
    if(event.isControlDown) then removeConnections()

  // We don't want to drag the component node if we have pressed on the socket
  this.onMouseDragged = event => event.consume()

  // TODO: Interaction with synth logic
  def disconnect(connectorLine:ConnectorLine):Unit =
    this.connections -= connectorLine
  def removeConnections():Unit=
    println(this.connections.map(_.toString))
    this.connections.toVector.foreach(_.delete())

end LineSocket
// House static variables
object LineSocket:
  // A bit of a hack to validate the socket when connecting. Keep track of the java type version
  // of the object for identity comparison, and of the original type to access its members.
  def setLastDragSource(scalaVer:LineSocket, javaVer:javafx.scene.layout.StackPane) =
    lastDragSource = Some(scalaVer, javaVer)
  private var lastDragSource:Option[(LineSocket, javafx.scene.layout.StackPane)] = None
end LineSocket


class ConnectorLine(start:LineSocket, end:LineSocket, canvas:SynthCanvas) extends Line:

  // Add to parent
  canvas.children += this

  // Input cannot connect to two separate. Check that this holds.
  val a = Try{
    start.addConnection(this)
    end.addConnection(this)
  }
  a match
    case Failure(exception) =>
      delete()
    case _ => ()

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




