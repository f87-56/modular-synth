package SynthLogic

import io.circe.{Codec, Decoder, Encoder, HCursor, Json}

import java.security.InvalidParameterException
import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

sealed trait SignalType[A]
given intSignal:SignalType[Int]()
given doubleSignal:SignalType[Double]()
given boolSignal:SignalType[Boolean]()
given stringSignal:SignalType[String]()
given doubleVecSignal:SignalType[Vector[Double]]()


// TODO: Add upper and lower bounds
/**
 *
 * @param name The visible name of this parameter
 * @param description A description for the parameter
 * @param takesInput Can this parameter take input from another SynthComponent?
 * @param defaultValue Default value to be given if no input exists
 * @param input The () SynthComponent that feeds data to this parameter. WARNING! THIS FIELD IS MUTABLE
 * @tparam T  The signal type of this parameter
 */
case class Parameter[T](name: String, description: String, takesInput: Boolean = true,
                         private var _defaultValue: T,
                                     parent: SynthComponent[_],
                                     private[this] var input: Option[SynthComponent[T]] = None)
                       (using signalType:SignalType[T]):

  parent.addParameter(this)

  def value:T =
    // Input is being computed, cannot use it for a value (stack overflow)
    input.map(_.output.getOrElse(defaultValue)).getOrElse(defaultValue)


  /**
   * A getter for input.
   * @return
   */
  def getInput:Option[SynthComponent[T]] = input

  // Getter, setter For defaultValue
  def defaultValue: T = _defaultValue

  // NOT TYPE-SAFE
  def defaultValue_= (newVal:Any): Unit =
    // A hack to circumvent compile errors. We'll have to trust that the data is of the right type.
    // Since the defaultValue is not used in program logic, the sin may not be quite so cardinal.
    // Sorry scala :(
    newVal match
      case a:T=>
        _defaultValue = a

  /**
   * WARNING! AN EFFECTFUL FUNCTION! This is one of the few places I'll allow it in the synth structure.
   * Connect a SynthComponent to this parameter. This should be THE ONLY method used for this purpose.
   *
   * Does nothing and returns a failure if the newComponent does not belong to the same synth.
   *
   * @param newInput The new input component
   */
  /*
  @targetName("connect")
  infix def <==(newInput:SynthComponent[T]):Try[Int] =
    if(!takesInput || (newInput.host != parent.host)) then Failure(IllegalArgumentException())
    else
      newInput.addConnection(this)
      input = Some(newInput.asInstanceOf[SynthComponent[T]])
      Success(1)*/

  /**
   * TODO: Refaactor this class????
   * @param newInput
   * @tparam A
   * @return
   */
  @targetName("connect")
  infix def <==[A](newInput: SynthComponent[A]): Try[Int] =
    if (!takesInput || (newInput.host != parent.host)) then
      Failure(IllegalArgumentException())
    else
      Try {
        // Type check
        if newInput.signalType == this.signalType then
          // Disconnect old one
          this.x()

          //println(newInput)
          newInput.addConnection(this)

          // anti-pattern
          input = Some(newInput.asInstanceOf[SynthComponent[T]])
          //println("NEW INPUT: " + input)
          1
        else
          println("ARRRR ME HEART STOPPED")
          throw InvalidParameterException()
      }

  /**
   * WARNING! AN EFFECTFUL FUNCTION
   * Disconnect the current input from this parameter
   * "Cut" the line feeding into this parameter (x)
   */
  @targetName("cut")
  def x(): Unit =
    //println("DISCONNECTING:" + name + ", " + input.getOrElse("..."))
    input.foreach(_.disconnect(this))
    //println("Remaining conns: " + input.map(_.connections).getOrElse("..."))
    input = None

  override def toString: String =
    this.name + ", " + this.signalType + ", " + this.input.map(_.serializationTag)


end Parameter
object Parameter:
  given Encoder[Parameter[_]] = (a: Parameter[_]) => Json.obj(
    // Index of the input. If none, -1.
    ("Index", Json.fromInt(a.getInput.map(b => b.host.components.indexOf(b)).getOrElse(-1))),
    // Encode them all, worry about validity when decoding
    ("DefaultValue", Json.fromString(a.defaultValue.toString))
  )

  // Only gives info, does not construct a Parameter.
  given Decoder[(Int,String)] = (c: HCursor) => for
    index <- c.downField("Index").as[Int]
    defaultVal <- c.downField("DefaultValue").as[String]
  yield
    (index, defaultVal)



end Parameter

/**
 * For parameters where discrete choices are presented
 */
trait EnumerableParam(choices:String*):
  this: Parameter[Int] =>
  def enumValue:String =
    choices.lift(this.value).getOrElse("")