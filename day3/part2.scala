package sol2

import scala.util.matching.Regex
import scala.util.CommandLineParser
import scala.io.Source
import scala.collection.BufferedIterator

val mulPattern: Regex = """mul\(\d{1,3},\d{1,3}\)""".r
val enableDisableString: Regex = """do\(\)|don't\(\)""".r
val nums: Regex = """\d+""".r

def readFile(debug: Boolean): String =
  val fileName = if (debug) "./test_input.txt" else "input.txt"
  Source.fromFile(fileName).getLines().mkString(" ")

sealed trait MyType

case class DoType(toggle: Boolean) extends MyType
case class MulType(num: Int) extends MyType

trait Processor[T <: MyType]:
  def process(inp: String): T

given Processor[DoType] with
  def process(inp: String): DoType = inp.match {
    case "do()"    => DoType(true)
    case "don't()" => DoType(false)
    case _         => throw new RuntimeException("Undefined match")
  }

given Processor[MulType] with
  def process(inp: String): MulType =
    MulType(nums.findAllIn(inp).map(_.toInt).reduce(_ * _))

def pattern[T <: MyType: Processor](x: Regex)(y: String): Iterator[(T, Int)] =
  x.findAllMatchIn(y)
    .map(s => (summon[Processor[T]].process(s.toString()), s.start))

def merge[A](
    x: BufferedIterator[(A, Int)],
    y: BufferedIterator[(A, Int)]
): Iterator[A] = {
  var ret: Vector[A] = Vector.empty
  while (!x.isEmpty && !y.isEmpty) {
    val (x_val, x_ind) = x.head
    val (y_val, y_ind) = y.head
    ret = Ordering.Int.compare(x_ind, y_ind) match {
      case n if n < 0 => {
        ret :+ x.next()._1
      }
      case 0 => {
        y.next()
        ret :+ x.next()._1
      }
      case n if n > 0 => {
        ret :+ y.next()._1
      }
    }
  }

  ret ++= x.toList.map(_._1)
  ret ++= y.toList.map(_._1)
  ret.iterator
}

def sol(inp: String): Unit =
  val muls = pattern[MulType](mulPattern)(inp).toSeq
  val dos = pattern[DoType](enableDisableString)(inp).toSeq
  val merged = merge(muls.iterator.buffered, dos.iterator.buffered).toList
  val combined = merged
    .foldLeft(
      (0, true)
    )((accState, value) =>
      val (acc, state) = accState
      (state, value).match {
        case (false, MulType(_)) => accState
        case (_, DoType(b))      => (acc, b)
        case (true, MulType(v))  => (acc + v, true)
      }
    )
    ._1
  println(combined)

@main def main(args: String*) =
  val input = readFile(args.length > 1)
  // println(enableDisableString.findAllIn(input).mkString(" "))
  sol(input)
