import scala.io.Source
import scala.util.matching.Regex

val digits: Regex = """\d+""".r

case class Instance(total: BigInt, nums: List[BigInt])

def processLine(line: String) =
    val dis = digits
        .findAllMatchIn(line)
        .map(s => BigInt.apply(s.toString()))
        .toList
    dis.headOption.map(Instance(_, dis.tail))

def readFile(fileName: String) =
    Source
        .fromFile(fileName)
        .getLines()
        .map(processLine)
        .flatMap(identity)
        .toList

def loop(total: BigInt)(root: List[BigInt], runningSum: BigInt = 0): Boolean =
    root.match {
        case Nil => runningSum == total
        case head :: next =>
            if runningSum > total then false
            else
                loop(total)(next, runningSum + head) || loop(total)(
                  next,
                  runningSum * head
                )
    }

def checkCondition(ins: Instance): Boolean = loop(ins.total)(ins.nums)

def part1(it: Iterator[Instance]): BigInt =
    it.filter(checkCondition).map(_.total).sum

def checkCondition2(ins: Instance): Boolean = loop2(ins.total)(ins.nums)

def concat(fst: BigInt, snd: BigInt): BigInt =
    val dSnd = snd.toString().length()
    BigInt(fst.toString() ++ ("0" * dSnd)) + snd

def loop2(total: BigInt)(root: List[BigInt], runningSum: BigInt = 0): Boolean =
    root.match {
        case Nil => runningSum == total
        case head :: next =>
            if runningSum > total then false
            else
                loop2(total)(next, runningSum + head) || loop2(total)(
                  next,
                  runningSum * head
                ) || loop2(total)(next, concat(runningSum, head))
    }

def part2(it: Iterator[Instance]): BigInt =
    it.filter(checkCondition2).map(_.total).sum

@main def main(fileName: String) =
    val ins = readFile(fileName)
    // println(part1(ins.iterator))
    println(part2(ins.iterator))
