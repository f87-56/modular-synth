package SynthGUI
import scala.annotation.unused
import scala.collection.mutable
import scala.collection.mutable.Buffer

// a log to output info to the user
object OutputLog {

  private val LOG_SIZE = 256
  val listeners:mutable.Buffer[LogListener] = mutable.Buffer()

  private val history:mutable.Queue[String] = mutable.Queue()

  def log(s:String):Unit =
    if(history.size >= LOG_SIZE) then
      history.dequeue()
      history += s
    else
      history += s
    listeners.foreach(_.onNewMessage())

  def lastLog: String =
    history.last
  
  @unused
  def wholeLog: Seq[String] = history.toVector
}

trait LogListener:
  OutputLog.listeners += this
  def onNewMessage():Unit
