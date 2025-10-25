package part1

import misc.*
import scala.util.matching.Regex
import scala.io.Source
import scala.collection.immutable.BitSet

class Instance(val map: SolMap, val updates: List[List[Int]])

def readFile: Instance =
    val fileLines = Source.fromFile("input.txt").getLines().iterator
    val (initial, uLines) = fileLines.span(p => reg.matches(p.strip()))
    val mapped = initial
        .map(x => {
            val y = reg.findAllIn(x)
            (y.group(1).toInt, y.group(2).toInt)
        })
        .toList

    val first100 = BitSet((0 until 100)*)
    val res =
        mapped.groupMapReduce(_._2)(x => first100 - x._1)(_ & _)
    val ups =
        uLines.drop(1).map(x => x.strip().split(",").map(_.toInt).toList).toList
    return Instance(res, ups)

def part1(ins: Instance) =
    val vals = ins.updates.filter(verifyUpdate(ins.map)).map(getMiddleValue)
    println(vals.sum)

@main def main =
    val ins = readFile
    part1(ins)
