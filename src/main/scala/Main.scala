import scala.annotation.tailrec
import scala.io.StdIn.{readInt, readLine}
import scala.util.{Failure, Success, Try}
import scala.math.{abs, pow, sqrt}

object Main {

  def main(args: Array[String]): Unit = {
    val (a, b) = Try(takeInterval()) match {
      case Success(value) => value
      case Failure(e) => {
        println(e, "set to Default: (-5.0, 5.0)")
        (-5.0, 5.0)
      }
    }
    println("Please enter n: ")
    val n = Try(readInt()) match {
      case Success(value) => value
      case Failure(except) => 3
    }
    val mult = takeEquation()

    print("Choose method")
    val res = readLine() match {
      case "1" => equalIntervals(a, b, n, mult, Double.MaxValue)
      case "2" => {
        val y = a + ((1 + sqrt (5) ) / 2) * (b - a)
        val z = a + b - y
        goldenRatio(a, b, y, z, mult, 0)
      }
      case _ => throw new UnknownError()
    }
    print("result, x = ", res)
  }

  @tailrec
  def takeInterval(): (Double, Double) = {
    print("Please enter String in format: '3,4'. Answer: ")
    val str = readLine().split(",").map(_.trim.toDouble).sorted
    str match {
      case Array(a, b) => (a, b)
      case _           => takeInterval()
    }
  }

  @tailrec
  def takeEquation(): Array[Double] = {
    print("Enter equation members in format '1,-2,5': ")
    Try(readLine().split(",").map(_.toDouble)) match {
      case Success(value)     => value
      case Failure(exception) => takeEquation()
    }
  }

  @tailrec
  def equalIntervals(a: Double, b: Double, n: Int, members: Array[Double], f0: Double): Double = {
    val x: Array[Double] = (for (i <- 0 to n)
      yield a + i * ((b - a) / (n + 1))
      ).toArray

    if (x.isEmpty) throw new UnsupportedOperationException("Array is empty")
    val y: Array[Double] = equationCalc(members, x)
    val min = y.min
    val index = y.indexOf(min)
    val len = y.length
    //Новое приближение
    val (a1, b1) = index match {
      case 0   => (x(index), x(index + 1))
      case len => (x(index - 1), x(index))
      case _   => (x(index - 1), x(index + 1))
    }

    if (f0 <= min) {
      x(index)
    } else {
      equalIntervals(a1, b1, n, members, min)
    }
  }

  def equationCalc(members: Array[Double], x: Array[Double]): Array[Double] = {
    for (elem <- x) yield calc(members.reverse, elem)
  }

  def calc(members: Array[Double], x: Double): Double = {
    val res = for (i <- members.indices) yield members(i) * pow(x, i)
    res.toArray.sum
  }

  @tailrec
  def goldenRatio(a: Double, b: Double, y: Double, z: Double, members: Array[Double], k: Int): Double = {
    val resY = calc(members, y)
    val resZ = calc(members, z)

    val (y1, z1, a1, b1) = (resY, resZ) match {
      case _ if(resY > resZ)  => (z, y+b-z, y, b)
      case _ if(resY <= resZ) => (a+z-y, y, a, z)
      case _ => throw new UnknownError()
    }

    val difference = abs(b1 - a1)
    difference match {
      case _ if(difference > 0.00001) => goldenRatio(a1, b1, y1, z1, members, k+1)
      case _ if(difference <= 0.00001) => (b1 + a1)/2
    }
  }
}
