error id: 518E383B51872938DF2ACA93024C3874
file://<WORKSPACE>/main.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.



action parameters:
offset: 2045
uri: file://<WORKSPACE>/main.scala
text:
```scala
import scala.util.matching.Regex
import scala.io.Source

type SolMap = Map[Int, CustomSet]
type Bool = Boolean
class Instance(val map: SolMap, val updates: List[List[Int]])

class CustomSet(val map: Vector[Bool]) {
    def add(el: Int): Option[CustomSet] = {
        if (el > 100) {
        None
        } else {
        val newMap = map.updated(el, true)
        Some(CustomSet(newMap))
        }
    }
    def remove(el: Int): Option[CustomSet] = {
        if (el > 100) {
            None
        } else {
            val newMap = map.updated(el, false)
            Some(CustomSet(newMap))
        }
    }

    def ++(that: CustomSet): CustomSet =
        CustomSet(this.map.zip(that.map).map(_ || _).toVector)

    def &(that: CustomSet): CustomSet =
        CustomSet(this.map.zip(that.map).map(_ && _).toVector)
    def +(other: Int): CustomSet = this.add(other).get
    def -(other: Int): CustomSet = this.remove(other).get

    override def toString(): String = map.toString
}

object CustomSet {
    def create(b: Bool = false): CustomSet = {
        val arr = Vector.fill[Bool](100)(b)
        CustomSet(arr)
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
    val res =
        mapped.groupMapReduce(_._2)(x => CustomSet.create(true) - x._1)(_ & _)
    val ups = uLines.drop(1).map(x => x.strip().split(",").map(_.toInt).toList).toList
    return Instance(res, ups)

def verifyUpdate(map: SolMap)(update: List[Int]): Bool =
    var initList = CustomSet.create(true) // Assume that all elements are allowed
        println("new")
    val table = update.foldLeft[Option[CustomSet]](Some(initList))((lis, x) => 
        println(List(x,
            lis.map(_.map.map(e => if (e) 1 else 0)).mkString(""), lis[@@]
            ))
        if (!map.contains(x)){
            lis
        } else {
            lis match{
                case None =>  None
                case Some(m) if (!m.map(x)) =>  None
                case Some(m) => Some(m & map(x))
            }
        })
    // println(table.map(_.map.reduce(_ && _)))
    table.isDefined

def getMiddleValue(update: List[Int]): Int = update(update.length / 2)

def part1(ins: Instance) =
    val vals = ins.updates.filter(verifyUpdate(ins.map)).map(getMiddleValue)
    println(vals)
    println(vals.sum)

@main def main =
    val ins = readFile
    part1(ins)

```


presentation compiler configuration:
Scala version: 3.7.1-bin-nonbootstrapped
Classpath:
<WORKSPACE>/.scala-build/day5_d5c0a6989e/classes/main [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.7.1/scala3-library_3-3.7.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/sourcegraph/semanticdb-javac/0.10.0/semanticdb-javac-0.10.0.jar [exists ], <WORKSPACE>/.scala-build/day5_d5c0a6989e/classes/main/META-INF/best-effort [missing ]
Options:
-Xsemanticdb -sourceroot <WORKSPACE> -Ywith-best-effort-tasty




#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:104)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:479)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1