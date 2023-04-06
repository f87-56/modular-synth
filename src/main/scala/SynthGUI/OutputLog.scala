package SynthGUI
import scala.collection.mutable
import scala.collection.mutable.Buffer

// a log to output info to the user
object OutputLog {

  val LOG_SIZE = 256
  val listeners:mutable.Buffer[LogListener] = mutable.Buffer()

  val history:mutable.Queue[String] = mutable.Queue()

  def log(s:String):Unit =
    if(history.size >= LOG_SIZE) then
      history.dequeue()
      history += s
    else
      history += s

  def lastLog = history.last
  
  def wholeLog = history.toVector
}

trait LogListener:
  OutputLog.listeners += this
  
  def onNewMessage:Unit
