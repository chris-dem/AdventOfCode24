package part1

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
    val fileLines = Source.fromFile("input.txt").getLines().iterator
    val (initial, uLines) = fileLines.span(p => reg.matches(p.strip()))
    val mapped = initial
        .map(x => {
            val y = reg.findAllIn(x)
            (y.group(1).toInt, y.group(2).toInt)
        })
        .toList
    val res =
        mapped.groupMapReduce(_._2)(x => CustomSet.create(true) - x._1)(_ & _)
    val ups =
        uLines.drop(1).map(x => x.strip().split(",").map(_.toInt).toList).toList
    return Instance(res, ups)

def verifyUpdate(map: SolMap)(update: List[Int]): Bool =
    var initList =
        CustomSet.create(true) // Assume that all elements are allowed
    val table = update.foldLeft[Option[CustomSet]](Some(initList))((lis, x) =>
        lis match {
            case None => None
            case Some(m) if (!m.map(x)) => {
                None
            }
            case Some(m) => {
                if (!map.contains(x)) {
                    Some(m)
                } else {
                    Some(m & map(x))
                }
            }
        }
    )
    // println(table.map(_.map.reduce(_ && _)))
    table.isDefined

def getMiddleValue(update: List[Int]): Int = update(update.length / 2)

def part1(ins: Instance) =
    val vals = ins.updates.filter(verifyUpdate(ins.map)).map(getMiddleValue)
    println(vals.sum)

@main def main =
    val ins = readFile
    part1(ins)
