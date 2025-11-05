import scala.io.Source
import scala.collection.immutable.TreeSet
import scala.util.matching.Regex

case class CellType(val char: Char)

type MapCell = Option[CellType]
type GridVec[A] = Vector[Vector[A]]
type MapType = GridVec[MapCell]

object MapDims {
    private var rows: Option[Int] = None
    private var cols: Option[Int] = None

    def setDims(rows: Int, cols: Int) = {
        this.rows = Some(rows)
        this.cols = Some(cols)
    }

    def getCols = cols.get
    def getRows = rows.get
}

object CellType {
    def fromMapCell(char: Char) =
        if char == '.' then None else Some(CellType(char))
}

case class MapIndex(x: Int, y: Int) {
    def +(other: MapIndex) = MapIndex(this.x + other.x, this.y + other.y)
    def -(other: MapIndex) = MapIndex(this.x - other.x, this.y - other.y)
}

object MapIndex {
    implicit val lexi: Ordering[MapIndex] = Ordering.by(x => (x.x, x.y))
}

def readFile(fileName: String) = {
    val map = Source
        .fromFile(fileName)
        .getLines()
        .map(_.map(CellType.fromMapCell).toVector)
        .toVector
    MapDims.setDims(map.length, map(0).length)
    map
}

def containsMap(map: MapType)(loc: MapIndex) =
    0 <= loc.x && loc.x < MapDims.getRows &&
        0 <= loc.y && loc.y < MapDims.getCols
    // && map(loc.x)(loc.y).isEmpty

def generateLocationSet(map: MapType) = map.zipWithIndex
    .flatMap((row, i) => row.zipWithIndex.map((c, j) => (c, i, j)))
    .collect { case (Some(a), i, j) => (a, i, j) }
    .groupMap(_._1)(x => MapIndex(x._2, x._3))

def part1GenPoints(map: MapType, element1: MapIndex)(element2: MapIndex) =
    val contMap = containsMap(map)
    var retLis = List.empty[MapIndex]
    retLis =
        if contMap(element1 - element2 + element1) then
            (element1 - element2 + element1) :: retLis
        else retLis
    retLis =
        if contMap(element2 - element1 + element2) then
            (element2 - element1 + element2) :: retLis
        else retLis
    TreeSet.from(retLis)

def part2GenPoints(map: MapType, element1: MapIndex)(element2: MapIndex) =
    val contMap = containsMap(map)
    val dir1 = element1 - element2
    val dir2 = element2 - element1
    var ret = TreeSet.empty[MapIndex]
    ret = Iterator
        .iterate(element1)(_ + dir1)
        .takeWhile(contMap)
        .foldLeft(ret)(_ + _)
    ret = Iterator
        .iterate(element2)(_ + dir2)
        .takeWhile(contMap)
        .foldLeft(ret)(_ + _)
    ret

def countSpots(
    foo: (MapType, MapIndex) => MapIndex => TreeSet[MapIndex]
)(map: MapType, element: MapIndex, rest: List[MapIndex]) =
    val comp = foo(map, element)
    rest.map(comp).foldLeft(TreeSet.empty[MapIndex])(_ ++ _)

def log[A](x: A) =
    println(x)
    x

@scala.annotation.tailrec
def countLocs(
    foo: (MapType, MapIndex) => MapIndex => TreeSet[MapIndex]
)(
    map: MapType,
    runningSum: TreeSet[MapIndex] = TreeSet.empty
)(charElems: List[MapIndex]): TreeSet[MapIndex] =
    charElems match {
        case Nil => (runningSum)
        case head :: next =>
            countLocs(foo)(map, runningSum ++ countSpots(foo)(map, head, next))(
              next
            )
    }

def sol(
    foo: (MapType, MapIndex) => MapIndex => TreeSet[MapIndex]
)(map: MapType): Int = {
    val locSet = generateLocationSet(map)
    val countFunc = countLocs(foo)(map)
    val els = locSet.values.map(countFunc `compose` (_.toList)).reduce(_ ++ _)
    els.size
}

@main def main(fileName: String) =
    val ins = readFile(fileName)
    // println(sol(part1GenPoints)(ins))
    println(sol(part2GenPoints)(ins))
