import scala.annotation.tailrec
import scala.io.StdIn.{readInt, readLine}
import scala.util.{Failure, Success, Try}
import scala.math.pow

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
      case "2" => Double.MaxValue //Todo Метод золотого сечения
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
}
