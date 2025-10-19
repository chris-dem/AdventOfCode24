package  sol1


import scala.util.matching.Regex
import scala.util.CommandLineParser
import scala.io.Source

val mulPattern: Regex = """mul\(\d{1,3},\d{1,3}\)""".r
val enableDisableString: Regex = """do\(\)|don't()""".r
val nums: Regex = """\d+""".r

def readFile(debug: Boolean) =
  val fileName = if (debug) "./test_input.txt" else "input.txt"
  val inputLine = Source.fromFile(fileName).getLines().mkString(" ")
  val res = mulPattern
    .findAllIn(inputLine)
    .map(s => nums.findAllIn(s).map(s => s.toInt).reduce(_ * _))
    .sum
  println(res)

@main def main(args: String*) =
  readFile(args.length > 1)
