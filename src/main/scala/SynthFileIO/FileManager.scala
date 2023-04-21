package SynthFileIO

import scala.io.Source.*
import scala.util.{Try, Using}

// Facilities for reading/writing files
class FileManager:

  // Just get the lines from the file.
  def readFile(fileName: String): Try[Iterator[String]] =
    Using(scala.io.Source.fromFile(fileName)) { fileSource =>
      fileSource.getLines()
    }

  def writeFile(fileDir: String) = ???

end FileManager
