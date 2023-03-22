package SynthLogic

import java.security.InvalidParameterException
import scala.annotation.targetName
import scala.util.{Failure, Success, Try}



// TODO: Add upper and lower bounds
/**
 *
 * @param name The visible name of this parameter
 * @param description A description for the parameter
 * @param takesInput Can this parameter take input from another SynthComponent?
 * @param defaultValue Default value to be given if no input exists
 * @param input The (optional) SynthComponent that feeds data to this parameter. WARNING! THIS FIELD IS MUTABLE
 * @tparam T  The signal type of this parameter
 */
case class Parameter[+T](name: String, description: String, takesInput: Boolean = true,
                                     defaultValue: T,
                                     parent: SynthComponent[_],
                                     private[this] var input: Option[SynthComponent[T]] = None):

  parent.addParameter(this)

  if(!takesInput) then input = None
  def value(context:RuntimeContext):T = input.map(_.output(context)).getOrElse(defaultValue)

  /**
   * A getter for input.
   * @return
   */
  def getInput:Option[SynthComponent[T]] = input

  /**
   * WARNING! AN EFFECTFUL FUNCTION! This is one of the few places I'll allow it in the synth structure.
   * Connect a SynthComponent to this parameter. This is THE ONLY method from which it can be done.
   *
   * @param newInput The new input component
   */
  @targetName("connect")
  infix def <==(newInput:SynthComponent[_]):Try[Int] =
    if(!takesInput) then Failure(IllegalArgumentException())
    newInput match
      case a:SynthComponent[T] =>
        newInput.addConnection(this)
        input = Some(a)
        Success(0)
      case _ => Failure(InvalidParameterException())

  /**
   * WARNING! AN EFFECTFUL FUNCTION
   * Disconnect the current input from this parameter
   * "Cut" the line feeding into this parameter (x)
   */
  @targetName("cut")
  def x(): Unit =
    input.foreach(_.disconnect(this))
    input = None


end Parameter
object Parameter:
end Parameter


/**
 * For parameters where discrete choices are presented
 */
trait EnumerableParam(choices:String*):
  this: Parameter[Int] =>
  def enumValue(runtimeContext: RuntimeContext):String =
    choices.lift(this.value(runtimeContext)).getOrElse("")