import SynthUtilities.SoundUtilities.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.security.InvalidParameterException
import scala.collection.mutable.Stack

class SoundUtilitiesSpec extends AnyFlatSpec with should.Matchers:

  "DbToVolume" should "Return the right values" in{
    dbToVolume(20) should equal (10.0 +- 0.01)
    dbToVolume(40) should equal (100.0 +- 0.01)
    dbToVolume(1) should equal (1.122 +- 0.01)
  }
  "volumeToB" should "Return the right values" in{
    volumeTodB(100) should equal (40.0 +- 0.01)
    volumeTodB(10) should equal (20.0 +- 0.01)
    volumeTodB(1.122) should equal (1.0 +- 0.01)
    volumeTodB(1) should equal (0)
  }
  it should "Throw an InvalidParameterException if given a negative input" in {
    assertThrows[IllegalArgumentException]{
      volumeTodB(-1)
    }
  }

  "noteFrequency" should "return the correct frequency for a given note" in {
    noteFrequency(60) should equal(261.63 +- 0.01)
    noteFrequency(72) should equal(523.25 +- 0.01)
    noteFrequency(69) should equal(440.0 +- 0.01)
    noteFrequency(81) should equal(880.0 +- 0.01)
    noteFrequency(84) should equal(1046.5 +- 0.1)
    noteFrequency(93) should equal(1760.0 +- 0.1)
    noteFrequency(21) should equal(27.50 +- 0.01)
    noteFrequency(0) should equal(8.18 +- 0.01)
    noteFrequency(1) should equal(8.66 +- 0.01)
    noteFrequency(-1) should equal(8.18 +- 0.01)
    noteFrequency(-15) should equal(8.18 +- 0.01)
    noteFrequency(127) should equal(12543.85 +- 0.01)
    noteFrequency(126) should equal(11839.82 +- 0.01)
    noteFrequency(128) should equal(12543.85 +- 0.01)
    noteFrequency(136) should equal(12543.85 +- 0.01)
  }