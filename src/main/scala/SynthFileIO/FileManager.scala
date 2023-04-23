package SynthFileIO

import SynthGUI.OutputLog

import java.io.*
import scala.util.{Try, Using}

// Facilities for reading/writing files
object FileManager:

  // Just get the lines from the file.
  // From https://dotty.epfl.ch/api/scala/util/Using$.html
  def readFile(fileName: String): Try[Seq[String]] =
    Using(new BufferedReader(new FileReader(fileName))) { reader =>
      Iterator.continually(reader.readLine()).takeWhile(_!=null).toSeq
    }


  // From https://alvinalexander.com/scala/how-to-write-text-files-in-scala-printwriter-filewriter/
  // Will create a new file if it does not already exist.
  // You might say having different signatures for read and right is bad, and you would be right.
  def writeFile(fileName: String, content: String): Unit =
  // Java facilities, we'll (begrudgingly) use try-catch.
    try
      val file = File(fileName)
      // throws
      val fw = new FileWriter(file)
      // does not throw
      val bw = new BufferedWriter(fw)

      try
        bw.write(content)
      finally
        bw.close()
        fw.close()
      end try
    catch
      // Log the errors
      case notFound: FileNotFoundException =>
        System.err.println("File not found: " + notFound)
        OutputLog.log("File not found: " + notFound)
      case e: IOException =>
        System.err.println("Unknown IOException: " + e)
        OutputLog.log("Unknown IOException: " + e)


end FileManager
