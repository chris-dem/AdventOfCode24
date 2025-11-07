import scala.io.Source
import scala.util.matching.Regex
import scala.collection.immutable.TreeMap

val rSpace = """\s+""".r

def processNumber(inp: (BigInt, BigInt)): List[(BigInt, BigInt)] =
    val (b, count) = inp
    if b == 0 then return List((BigInt(1), count))
    val bString = b.toString
    if bString.size % 2 == 0 then
        List(
          (BigInt(bString.slice(0, bString.size / 2)), count),
          (BigInt(bString.slice(bString.size / 2, bString.size)), count)
        )
    else List((b * 2024, count))

def readFile(fileName: String) =
    val string = Source.fromFile(fileName).getLines().next.trim()
    rSpace.split(string).map(x => BigInt(x)).toList

def sol1(inp: Map[BigInt, BigInt]) = (0 `until` 75)
    .foldLeft(inp)((op, _) => {
        op.iterator
            .flatMap(processNumber)
            .toList
            .groupMapReduce(_._1)(_._2)(_ + _)
    })
    .values
    .sum

@main def main(fileName: String) =
    val ins = readFile(fileName)
    val prep = ins.groupMapReduce(identity)(_ => BigInt(1))(_ + _)
    println(sol1(prep))
