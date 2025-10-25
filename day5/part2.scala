package part2

import misc.*
import scala.util.matching.Regex
import scala.io.Source
import scala.collection.immutable.VectorMap
import scala.collection.immutable.BitSet
import scala.collection.mutable.BitSet as MBit
import scala.collection.mutable.LongMap
import scala.collection.mutable.TreeMap
import scala.collection.mutable.PriorityQueue

type Stack[A] = List[A]

object GlobalCounter {
    private var counter: Int = 0
    def add(): Int =
        counter += 1
        counter

    def get(): Int = counter
    def reset(): Unit = counter = 0
}

class Instance(
    val map: SolMap,
    val outNeigh: SolMap,
    val updates: List[List[Int]]
)

val first100 = BitSet((0 until 100)*)
def readFile: Instance =
    val fileLines = Source.fromFile("ainput.txt").getLines().iterator
    val (initial, uLines) = fileLines.span(p => reg.matches(p.strip()))
    val mapped = initial
        .map(x => {
            val y = reg.findAllIn(x)
            (y.group(1).toInt, y.group(2).toInt)
        })
        .toList
    val res =
        mapped.groupMapReduce(_._2)(x => first100.excl(x._1))(_ & _)
    val other =
        mapped.groupMapReduce(_._1)(x => BitSet.empty `incl` x._2)(_ ++ _)
    val ups =
        uLines.drop(1).map(x => x.strip().split(",").map(_.toInt).toList).toList
    return Instance(res, other, ups)

def part1(ins: Instance) =
    val vals = ins.updates.filter(verifyUpdate(ins.map)).map(getMiddleValue)
    println(vals.sum)

def topSort_(
    relevant: BitSet,
    outNeigh: SolMap,
    root: Int,
    visited: BitSet
): (BitSet, Map[Int, Int]) =
    val ret = visited.incl(root)
    val all = (outNeigh.getOrElse(root, BitSet.empty) & relevant)
    var vis = visited.incl(root)
    var mapRet = Map.empty[Int, Int]
    for (el <- all) {
        if (!vis(el)) {
            val res = topSort_(relevant, outNeigh, el, vis)
            vis = res._1
            mapRet = mapRet ++ res._2
        }
    }
    mapRet += (root, GlobalCounter.add())
    (vis, mapRet)

def topSort(ins: Instance)(x: List[Int]): List[Int] =
    GlobalCounter.reset()
    val relevant = BitSet(x*)
    val root = x
        .filter(
          ins.map
              .mapValues(b => (first100 -- b) & relevant)
              .filter((k, inNeigh) => relevant.contains(k) && !inNeigh.isEmpty)
              .keySet
              .contains
              .andThen(!_)
        )
        .head
    val (_, ret) = topSort_(relevant, ins.outNeigh, root, BitSet.empty)
    x.sortBy(x => ret(x))
    // x

def log[A](x: A): A =
    println(x)
    x

def part2(ins: Instance) =
    val vals = ins.updates
        .filter(verifyUpdate(ins.map).andThen(!_))
        .map(topSort(ins))
        // .map(_.toString.andThen(println))
        .map(getMiddleValue)
    println(vals.sum)

@main def main =
    val ins = readFile
    // part1(ins)
    part2(ins)
