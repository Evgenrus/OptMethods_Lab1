import org.scalatest.FlatSpec
import scala.math.abs

class Test extends FlatSpec {

  "x^2 - 4x + 4 by EqualIntervals" should "x = 1" in {
    assert(abs(Main.equalIntervals(-5, 5, 5, Array(1, 4, 4), Double.MaxValue) - -2.0) < 0.0001)
  }

  "10x^3 + 3x^2 + x + 5 by EqualIntervals on [-5, 5]" should "-5.00" in {
    assert((Main.equalIntervals(-5, 5, 5, Array(10,3,1,5), Double.MaxValue) - -5.0) < 0.0001)
  }

}
