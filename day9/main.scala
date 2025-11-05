import scala.io.Source
import scala.collection.immutable.TreeMap
import scala.util.matching.Regex
import scala.annotation.tailrec

def readFile(fileName: String) =
    Source.fromFile(fileName).getLines().next()

def sol(inp: String) = ???

enum ReadMode {
    case FileMode
    case SpaceMode

    def toggle =
        this match {
            case FileMode  => SpaceMode
            case SpaceMode => FileMode
        }
}

@tailrec
def interleave[A](
    a: List[A],
    b: List[A],
    acc: List[A] = List.empty[A]
): List[A] =
    (a, b).match {
        case (Nil, _) => (b ++ acc).reverse
        case (_, Nil) => (a ++ acc).reverse
        case (ah :: at, bh :: bt) =>
            interleave(at, bt, bh :: ah :: acc)
    }

sealed trait UnitType {
    def toDebug: String
}
case class File(val fileId: Int, val width: Int) extends UnitType {
    override def toString(): String = List.fill(width)(fileId).mkString(",")
    override def toDebug = s"File(${fileId}, ${width})"
}
case class Space(val space: Int) extends UnitType {
    override def toString(): String = List.fill(space)(".").mkString(",")
    override def toDebug = s"Space(${space})"
}

def processInput(
    inp: List[Char]
) =
    val (filePart, spacePart) = inp.zipWithIndex.partition(_._2 % 2 == 0)
    val fileSeq =
        filePart.zipWithIndex.map(x => File(x._2, x._1._1.toInt - '0'.toInt))
    val spaceSeq = spacePart.map(x => Space(x._1.toInt - '0'.toInt))

    (fileSeq, spaceSeq)
    // interleave[UnitType](fileSeq, spaceSeq)

@tailrec
def loop(
    normal: List[(UnitType, Int)],
    reversed: List[(UnitType, Int)],
    acc: Seq[Int] = Seq.empty
): Seq[Int] =
    (normal, reversed).match {
        case (Nil, _) | (_, Nil)        => ???
        case (_, (Space(_), _) :: rest) => loop(normal, rest, acc)
        case ((_, a) :: _, (File(id, _), b) :: _) if a == b => acc :+ id
        case ((Space(_), a) :: _, (File(_, _), b) :: _) if a - 1 == b => acc
        case (_, (File(_, 0), _) :: rest) => loop(normal, rest, acc)
        case ((n1, a) :: _, (n2, b) :: _) if a > b => {
            println(List(n1, n2))
            println(List(a, b))
            ???
        }
        case ((File(h, n), _) :: rest, _) =>
            loop(rest, reversed, acc ++ List.fill(n)(h))
        case ((Space(0), _) :: rest, _) => loop(rest, reversed, acc)
        case ((Space(nSpace), nInd) :: rest, (File(rIp, rWidth), rInd) :: rRest)
            if nSpace > 0 =>
            loop(
              (Space(nSpace - 1), nInd + 1) :: rest,
              (File(rIp, rWidth - 1), rInd - 1) :: rRest,
              acc :+ rIp
            )
        case _ => ???
    }

def solPart1(a: List[UnitType], b: List[UnitType]) =
    val inp = interleave(a, b)
    val lens = inp
        .map(x =>
            x match {
                case File(_, x) => x
                case Space(x)   => x
            }
        )
        .scanLeft(0)(_ + _)
    val normal = inp.zip(lens)
    val reversed = (inp.zip(lens.tail.map(_ - 1))).reverse
    val res = loop(normal, reversed)
    res.zipWithIndex.map((_ * _)).map(x => BigInt(x)).sum

@tailrec
def loopPart2(
    fileSeq: List[(File, Int)],
    spaceMap: TreeMap[Int, Int],
    acc: Vector[Vector[File]]
): (Vector[Vector[File]], TreeMap[Int, Int]) =
    if fileSeq.isEmpty then {
        (acc, spaceMap)
    } else {
        val retIndx = fileSeq.length - 1
        val (a @ File(id, width), indx) = fileSeq.head
        spaceMap.view
            .filter(x => x._1 < indx && x._2 >= width)
            .minOption match {
            case None =>
                loopPart2(
                  fileSeq.tail,
                  spaceMap,
                  acc.updated(retIndx, a +: acc(retIndx))
                )
            case Some((i, w)) => {
                val newAcc = acc.updated((i -1)/ 2, acc((i - 1)/2) :+ a)
                val newSpaceMap = if w == width then {
                    spaceMap.removed(i)
                } else {
                    spaceMap.updated(i, w - width)
                }
                loopPart2(fileSeq.tail, newSpaceMap + (indx -> width), newAcc)
            }
        }
    }

@tailrec
def merge[A](
    a: List[(Int, A)],
    b: List[(Int, A)],
    acc: List[A] = List.empty
): List[A] =
    (a, b).match {
        case (Nil, _) => acc.reverse ++ b.map(_._2)
        case (_, Nil) => acc.reverse ++ a.map(_._2)
        case ((aInd, aVal) :: arest, (bInd, bVal) :: brest) if aInd < bInd =>
            merge(arest, b, aVal :: acc)
        case ((aInd, aVal) :: arest, (bInd, bVal) :: brest) if aInd >= bInd =>
            merge(a, brest, bVal :: acc)
        case _ => ???

    }

def solPart2(fileSeq: List[File], spaceSeq: List[Space]) = {
    val (fileIndexes, spaceIndexes) = interleave(fileSeq, spaceSeq).zipWithIndex
        .map(_._2)
        .partition(_ % 2 == 0)
    val treeMap = TreeMap.from(
      spaceIndexes.zip(spaceSeq).map((a, b) => (a, b.space))
    )
    val tempAcc = fileSeq.map(_ => Vector.empty).toVector
    val (fileVecs, treeVecs) = {
        val (fileVecs, treeVecs) =
            loopPart2(fileSeq.zip(fileIndexes).reverse, treeMap, tempAcc)
        val fileMod = fileVecs
            .zip(fileIndexes)
            .filterNot(_._1.isEmpty)
            .map((x, y) => (y, x))
            .toList
        val treeMod = treeVecs.toList.map((k, v) => (k, Vector(Space(v))))
        (fileMod, treeMod)
    }
    val merged = merge(fileVecs, treeVecs)
    merged.flatten
        .flatMap(x =>
            x match {
                case File(i, w) => List.fill(w)(i)
                case Space(w)   => List.fill(w)(0)
            }
        )
        .map(x => BigInt(x))
        .zipWithIndex
        .map(_ * _)
        .sum

}

@main def main(fileName: String) =
    val ins = readFile(fileName)
    val (fileSeq, spaceSeq) = processInput(ins.toCharArray().toList)
    // println(solPart1(fileSeq, spaceSeq))
    println(solPart2(fileSeq, spaceSeq))
