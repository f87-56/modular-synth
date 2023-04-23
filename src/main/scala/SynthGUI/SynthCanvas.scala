package SynthGUI

import SynthLogic.ModularSynthesizer
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax.*
import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.Region
import scalafx.scene.shape.Rectangle
import ModularSynthesizer.{given_Decoder_ModularSynthesizer, *}
import GUISynthComponent.given_Decoder_Int_Double_Double

import scala.util.Try
// LayoutChildren does not work in the scala wrapper
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color

// from http://fxexperience.com/2014/05/resizable-grid-using-canvas/
class SynthCanvas(private var _synth:ModularSynthesizer) extends Pane:

  this.setMinSize(SynthCanvas.canvasSize._1, SynthCanvas.canvasSize._2)
  val grid = SynthCanvas.bgGrid

  this.children += grid
  grid.toBack()

  private val components:scala.collection.mutable.Set[GUISynthComponent[_]] =
    scala.collection.mutable.Set()

  def addComponent(comp:GUISynthComponent[_]):Unit =
    this.children += comp
    components += comp

  def removeComponent(comp:GUISynthComponent[_]):Unit =
    this.children -= comp
    components -= comp

  // accessor
  def synth: ModularSynthesizer = _synth

  // prevent children from going out of bounds
  private val clip:Rectangle = Rectangle(this.getWidth + 500, this.getHeight + 500)
  //this.setClip(clip)

  // If the node is out of bounds, set it into bounds.
  def restrictToBounds(node:Region): Unit =
    // Bounds of background grid in local coords
    val bounds =  this.sceneToLocal(grid.localToScene(grid.getLayoutBounds))
    node.translateX = math.max(math.min(node.getTranslateX, bounds.getMaxX - node.getWidth), bounds.getMinX)
    node.translateY = math.max(math.min(node.getTranslateY, bounds.getMaxY - node.getHeight), bounds.getMinY)


  // The latest known mouse position
  private var localMousePos_ = (0.0,0.0)
  def localMousePos: (Double, Double) = localMousePos_



  this.onMouseMoved = event =>
    this.localMousePos_ = (event.getX, event.getY)

end SynthCanvas
object SynthCanvas:

  private val XSpacing = 50.0
  private val YSpacing = 40.0
  private val LineThickness = 3
  val canvasSize: (Int, Int) = (4000, 3000)

  private val bgGrid = {
    // We know beforehand how big the pane is.
    val w = canvasSize._1
    val h = canvasSize._2

    val grid = new Canvas(canvasSize._1, canvasSize._2)

    // Grid should not catch any mouse events
    grid.setMouseTransparent(true)

    // drawing
    val g = grid.getGraphicsContext2D
    g.clearRect(0, 0, w, h)
    g.setFill(Color.gray(0, 0.5))
    g.fillRect(0, 0, w, h)
    g.setFill(Color(1, 1, 1, 0.2))


    val a = LazyList.iterate(0.0)(_ + XSpacing)
    val b = LazyList.iterate(0.0)(_ + YSpacing)
    for x <- a.takeWhile(_ < w)
        y <- b.takeWhile(_ < h) do
      // 1.6180... is the Golden ratio. Such an offset Looks prettier and more coherent!A
      val offsetY = if (y % (2 * YSpacing)) == 0 then XSpacing / 1.618034 else 0
      //val offsetY = 0
      // We have a dotted background for the meantime
      g.fillOval(x - LineThickness + offsetY,
        y - LineThickness, 2 * LineThickness, 2 * LineThickness)

    grid
  }


  given Encoder[SynthCanvas] = (a:SynthCanvas) => Json.obj(
    ("Synth", a.synth.asJson),
    // Houses the optional info of where the nodes lie
    ("VisualComponents", a.components.asJson)
  )
  given Decoder[SynthCanvas] = (c: HCursor) => for
    synth <- c.downField("Synth").as[ModularSynthesizer](given_Decoder_ModularSynthesizer)
    visualComponents <- c.downField("VisualComponents").as[List[(Int,(Double,Double))]]
  yield
    val canvas = SynthCanvas(synth)
    val guiComps = synth.components.map(a => GUISynthComponent(canvas, a))
    guiComps.foreach(canvas.addComponent(_))  // Add the components to canvas

    // The lines
    val componentBros = synth.components zip guiComps
    //val paramBros = componentBros.map(a => (a._1.parameters, a._2.parameters))

    componentBros.foreach(
      a =>
        val paramBros = a._1.parameters zip a._2.parameters
        paramBros.foreach(pr =>
          val inputComp = pr._1.getInput
          val guiInputComp = inputComp.flatMap(c => componentBros.find(_._1 eq c).map(_._2))
          guiInputComp.foreach(cc =>
            ConnectorLine(cc.outputSocket, pr._2.inputSocket, canvas)
          )
        )
        /*pr =>
          val inputComp = pr._1.getInput
          val guiInputComp =
            inputComp.flatMap(c => componentBros.find(_._1 eq c).map(_._2))
          guiInputComp.foreach(a =>
            ConnectorLine(a.outputSocket, pr._2.inputSocket, canvas)
          ))*/)

    // Set the right positions
    visualComponents.foreach(a => guiComps.lift(a._1).foreach{ b =>
      val posInParent = b.localToParent(a._2._1, a._2._2)
      b.translateX = posInParent.x
      b.translateY = posInParent.y

    })
    canvas

end SynthCanvas


