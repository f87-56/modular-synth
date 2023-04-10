package SynthGUI

import SynthGUI.GUISynthComponent.DragContext
import scalafx.scene.layout.{HBox, StackPane, VBox}
import SynthLogic.SynthComponent
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.Label
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.{Circle, Rectangle}

import java.net.Socket
import scala.util.Try

class GUISynthComponent[T](val canvas:SynthCanvas) extends VBox:
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
      children +=
        new LineSocket()
        alignment = Pos.BaselineRight
  style =
    """
      |-fx-background-color: #f4f4f4;
      |    -fx-border-color: black;
      |    -fx-border-width: 1px;
      |    -fx-border-radius: 5px;
      |    -fx-background-radius: 5px;
      |    -fx-effect: dropshadow(three-pass-box, rgba(0,0,0,0.5), 10, 0, 0, 0);
      |""".stripMargin

  // Make the node draggable. From https://docs.oracle.com/javafx/2/events/filters.htm#BCFFCAIH
  this.onMousePressed = event =>
    val posInParent = localToParent(event.getX, event.getY)
    DragContext.mouseAnchorX = posInParent.x
    DragContext.mouseAnchorY = posInParent.y
    DragContext.initialTranslateX = this.getTranslateX
    DragContext.initialTranslateY = this.getTranslateY
    event.consume()

  this.onMouseDragged = event =>
    val posInParent = localToParent(event.getX, event.getY)
    this.translateX = DragContext.initialTranslateX + posInParent.x - DragContext.mouseAnchorX
    this.translateY = DragContext.initialTranslateY + posInParent.y - DragContext.mouseAnchorY

    event.consume()
    canvas.restrictToBounds(this)

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

class GUISynthParameter[T]() extends HBox:
  this.padding = Insets(20)
  spacing = 5
  private val inputSocket = new LineSocket():
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

end GUISynthParameter

class LineSocket extends StackPane:
  private val SocketSize = 3.0
  private val plugSocket = Circle(SocketSize)
  plugSocket.stroke = Color.Black
  plugSocket.fill = Color.White
  style =
    """
      |-fx-background-color: #f4f4f4;
      |    -fx-background-radius: 5px;
      |    -fx-padding: 5px;
      |""".stripMargin

  this.children = plugSocket
  def plug() =
    plugSocket.fill = Color.Black
  def unplug() =
    plugSocket.fill = Color.White
end LineSocket
