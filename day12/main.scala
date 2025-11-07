import scala.io.Source
import scala.util.matching.Regex
import scala.collection.immutable.TreeMap

def readFile(fileName: String) =
    Source.fromFile(fileName).getLines()
@main def main(fileName: String) =
    val ins = readFile(fileName)
    println(ins)