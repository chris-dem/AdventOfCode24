error id: file://<WORKSPACE>/main.scala:[203..206) in Input.VirtualFile("file://<WORKSPACE>/main.scala", "import scala.util.matching.Regex
import scala.io.Source

type SolMap = Map[Int, Set[Int]]
class Instance(map: SolMap, updates: List[List[Int]])

class CustomSet(map: Array[Int])

object CustomSet {
	def new() {
		val arr = new Array[boolean](100)
	}
}




val reg: Regex = """(\d+)\|(\d+)""".r

def readFile: Instance =
  val fileLines = Source.fromFile("test_input.txt").getLines().iterator
  val (initial, uLines) = fileLines.span(p => reg.matches(p.strip()))
  val mapped = initial
    .map(x => {
      val y = reg.findAllIn(x)
      (y.group(1).toInt, y.group(2).toInt)
    })
    .toList
  val res = mapped.groupMapReduce(_._1)(x => Set(x._2))(_ ++ _)
  val ups = uLines.map(x => x.strip().split(",").map(_.toInt).toList).toList
  return Instance(res, ups)


def part1(ins: Instance) = 
	val 

@main def main =
  val ins = readFile
")
file://<WORKSPACE>/file:<WORKSPACE>/main.scala
file://<WORKSPACE>/main.scala:10: error: expected identifier; obtained new
	def new() {
     ^
#### Short summary: 

expected identifier; obtained new