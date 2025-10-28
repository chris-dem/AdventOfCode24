package partB

import scala.io.Source
import scala.collection.immutable.TreeSet
import scala.compiletime.ops.boolean

type Wall = Unit
type Grid = Vector[CellType]

enum Direction {
    case Up, Down, Left, Right

    def reverse = this.match
        case Up    => Down
        case Down  => Up
        case Left  => Right
        case Right => Left

    def turn = this match
        case Up    => Right
        case Down  => Left
        case Left  => Up
        case Right => Down

}

enum CellType:
    case Wall
    case Empty
    case Explorer(direction: Direction)

object CellType {
    def fromChar(c: String): CellType = c.match {
        case "#" => CellType.Wall
        case "v" => CellType.Explorer(Direction.Up)
        case ">" => CellType.Explorer(Direction.Right)
        case "<" => CellType.Explorer(Direction.Left)
        case "^" => CellType.Explorer(Direction.Down)
        case _   => CellType.Empty
    }
}

case class MapIndex(x: Int, y: Int) {

    def reverse = MapIndex(-x, -y)

    def +(other: MapIndex): MapIndex = MapIndex(x + other.x, y + other.y)
    def -(other: MapIndex): MapIndex = this + other.reverse

    def +(other: Direction): MapIndex = other match {
        case Direction.Down  => this + MapIndex(-1, 0)
        case Direction.Up    => this + MapIndex(+1, 0)
        case Direction.Left  => this + MapIndex(0, +1)
        case Direction.Right => this + MapIndex(0, -1)
    }

    def -(other: Direction): MapIndex = this + other.reverse
}

object MapIndex {
    implicit val byXY: Ordering[MapIndex] = Ordering.by(m => (m.x, m.y))
}

class Instance(val map: GridStruct, val player: MapIndex, val dir: Direction)

object MapDims {
    private var rows: Option[Int] = Option.empty[Int]
    private var cols: Option[Int] = Option.empty[Int]

    def setDims(x: Int, y: Int): Unit = {
        rows = Some(x)
        cols = Some(y)
    }

    def getRows = rows.get
    def getCols = cols.get

    def getLim(mapIndex: MapIndex, dir: Direction) =
        dir.match
            case Direction.Up    => MapIndex(getRows - 1, mapIndex.y)
            case Direction.Down  => MapIndex(0, mapIndex.y)
            case Direction.Left  => MapIndex(mapIndex.x, 0)
            case Direction.Right => MapIndex(mapIndex.x, getCols - 1)

    def convertIndex(ind: Int): MapIndex =
        val cols = getCols
        MapIndex(ind / cols, ind % cols)
}

class GridStruct(
    val rowMap: Vector[TreeSet[Int]],
    val colMap: Vector[TreeSet[Int]]
) {
    def findNext(indx: MapIndex, direction: Direction): Option[MapIndex] =
        direction match {
            case Direction.Up =>
                colMap(indx.y)
                    .from(indx.x)
                    .headOption
                    .map(MapIndex(_, indx.y))
                    .map(_ + MapIndex(-1, 0))
            case Direction.Down =>
                colMap(indx.y)
                    .to(indx.x)
                    .lastOption
                    .map(MapIndex(_, indx.y))
                    .map(_ + MapIndex(1, 0))
            case Direction.Right =>
                rowMap(indx.x)
                    .to(indx.y)
                    .lastOption
                    .map(MapIndex(indx.x, _))
                    .map(_ + MapIndex(0, 1))
            case Direction.Left =>
                rowMap(indx.x)
                    .from(indx.y)
                    .headOption
                    .map(MapIndex(indx.x, _))
                    .map(_ + MapIndex(0, -1))
        }

    def +(other: MapIndex): GridStruct =
        GridStruct(
          rowMap.updated(other.x, rowMap(other.x) + other.y),
          colMap.updated(other.y, colMap(other.y) + other.x)
        )
}

object GridStruct {
    def init = GridStruct(
      rowMap = Vector.fill(MapDims.getRows)(TreeSet.empty),
      colMap = Vector.fill(MapDims.getCols)(TreeSet.empty)
    )
}

def readFile(fileName: String): Instance =
    val map = {
        var map = Source
            .fromFile(fileName)
            .getLines()
            .map(_.trim().split("").map(CellType.fromChar).toVector)
            .toVector

        MapDims.setDims(map.length, map(0).length)
        map.flatten
    }
    val explorerIndex = map
        .indexWhereOption(_.isInstanceOf[CellType.Explorer])
        .map(x => (x, MapDims.convertIndex(x)))
        .get

    val CellType.Explorer(explorerDir) = map(explorerIndex._1): @unchecked

    val gridStruct = map.zipWithIndex
        .filter(_._1 == CellType.Wall)
        .map(MapDims.convertIndex compose (_._2))
        .foldLeft(GridStruct.init)(_ + _)
    Instance(gridStruct, explorerIndex._2, explorerDir)

extension [A](seq: Seq[A])
    def indexWhereOption(pred: A => Boolean): Option[Int] =
        val idx = seq.indexWhere(pred)
        if (idx >= 0) Some(idx) else None

@scala.annotation.tailrec
def loop(grid: GridStruct)(
    indx: MapIndex,
    dir: Direction,
    visited: Option[List[MapIndex]] = None
): List[MapIndex] =
    val seq = visited match {
        case None     => List(indx)
        case Some(el) => el
    }
    val next = grid.findNext(indx, dir)
    next match {
        case Some(ind) => loop(grid)(ind, dir.turn, Some(ind :: seq))
        case None      => (MapDims.getLim(indx, dir) :: seq).reverse
    }

case class Line(pointA: MapIndex, pointB: MapIndex, dir: Direction) {
    def |=|(other: Line): Boolean =
        (isHorizontal == other.isHorizontal) && (pointA.x == other.pointA.x || pointA.y == other.pointA.y)

    def -|-(other: Line): Boolean = isHorizontal && other.isHorizontal
    def <=>(other: Line): Boolean =
        val sameLine =
            (this -|- other && (this.pointA.y <= other.pointA.y && other.pointA.y <= this.pointB.y ||
                other.pointA.y <= this.pointA.y && other.pointA.y <= other.pointB.y))
        val sameCol =
            (this.pointA.x <= other.pointA.x && other.pointA.x <= this.pointB.x ||
                other.pointA.x <= this.pointA.x && other.pointA.x <= other.pointB.x)
        (this |=| other) && (sameLine || sameCol)

    def toPoints: Iterator[MapIndex] =
        if isHorizontal then
            (pointA.y to pointB.y).map(MapIndex(pointA.x, _)).toIterator
        else (pointA.x to pointB.x).map(MapIndex(_, pointA.y)).toIterator

    def isHorizontal = dir == Direction.Right || dir == Direction.Left
}

object Line {
    def createLine(x: MapIndex, y: MapIndex): Option[Line] =
        if (x.x != y.x && x.y != y.y) {
            None
        } else {
            Some(if (x.x == y.x) {
                val dir = if x.y <= y.y then Direction.Left else Direction.Right
                Line(
                  pointA = MapIndex(x.x, x.y.min(y.y)),
                  pointB = MapIndex(x.x, x.y.max(y.y)),
                  dir
                )
            } else {
                val dir = if x.x <= y.x then Direction.Up else Direction.Down
                Line(
                  pointA = MapIndex(x.x.min(y.x), y.y),
                  pointB = MapIndex(x.x.max(y.x), y.y),
                  dir
                )
            })
        }
}

def part2(ins: Instance): Unit =
    var currentPosition: Option[MapIndex] = Some(ins.player)
    val result = loop(ins.map)(ins.player, ins.dir).toList
    var lines =
        result
            .zip(result.tail)
            .map(Line.createLine)
            .foldRight(Option(List.empty[Line]))((b, acc) =>
                (acc, b) match {
                    case (None, _)            => None
                    case (_, None)            => None
                    case (Some(lis), Some(x)) => Some(x :: lis)
                }
            )
            .get
    val res = lines
        .flatMap(_.toPoints)
        .to(TreeSet)

    println(res.size)

@main def main(fileName: String) =
    val ins = readFile(fileName)
    part2(ins)
