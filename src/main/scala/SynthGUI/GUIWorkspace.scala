package SynthGUI

import scalafx.scene.paint.Color.*
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Rectangle
import scalafx.scene.transform.*

// Represents the "drafting table" that the synths are built in
class GUIWorkspace extends Pane:

  this.children += new Rectangle:
    x = 100
    y = 100
    width = 50
    height = 50
    fill = Blue

  private var viewPos = (0,0)

  val a = this.width.value
  println(a)

  this.onScroll = _ => this.children.foreach(_.getTransforms.add(new Scale(0.5,0.5)))
end GUIWorkspace

// A class to handle our positions
case class Vec2(x:Double, y:Double)