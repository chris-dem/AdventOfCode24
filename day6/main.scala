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

class MapIndex(val x: Int, val y: Int) {

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
    override def toString(): String = s"MapIndex($x, $y)"
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
            case Direction.Up    => MapIndex(getRows, mapIndex.y)
            case Direction.Down  => MapIndex(-1, mapIndex.y)
            case Direction.Left  => MapIndex(mapIndex.x, -1)
            case Direction.Right => MapIndex(mapIndex.x, getCols)

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
                    .to(indx.x)
                    .lastOption
                    .map(MapIndex(indx.x, _))
                    .map(_ + MapIndex(0, 1))
            case Direction.Left =>
                rowMap(indx.x)
                    .from(indx.x)
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

def readFile: Instance =
    val map = {
        var map = Source
            .fromFile("test_input.txt")
            .getLines()
            .map(_.trim().split("").map(CellType.fromChar).toVector)
            .toVector

        MapDims.setDims(map.length, map(0).length)
        map.flatten
    }
    val explorerIndex = map
        .indexWhereOption(_.match {
            case CellType.Explorer(_) => true
            case _                    => false
        })
        .map(x => (x, MapDims.convertIndex(x)))
        .get
    val explorerDir = map(explorerIndex._1).match
        case CellType.Explorer(direction) => direction
        case _                            => ???

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

class Line(val pointA: MapIndex, val pointB: MapIndex) {
    def |=|(other: Line): Boolean =
        (isHorizontal == other.isHorizontal) && (pointA.x == other.pointA.x || pointA.y == other.pointA.y)
    def <>(other: Line): Option[Line] =
        val checks = this |=| other
        if (!checks)
            None
        else if (isHorizontal)
            Some(
              Line(
                MapIndex(pointA.x, pointA.y.min(other.pointA.y)),
                MapIndex(pointA.x, pointB.y.max(other.pointB.y))
              )
            )
        else
            Some(
              Line(
                MapIndex(pointA.x.min(other.pointA.x), pointA.y),
                MapIndex(pointB.x.max(other.pointB.x), pointA.y)
              )
            )

    def isHorizontal = pointA.x == pointB.x
}

object Line {
    def createLine(x: MapIndex, y: MapIndex): Option[Line] =
        if (x.x != y.x && x.y != y.y) {
            None
        } else {
            Some(if (x.x == y.x) {
                Line(
                  pointA = MapIndex(x.x, x.y.min(y.y)),
                  pointB = MapIndex(x.x, x.y.max(y.y))
                )
            } else {
                Line(
                  pointA = MapIndex(x.x.min(y.x), y.y),
                  pointB = MapIndex(x.x.max(y.x), y.y)
                )
            })
        }
}

def part1(ins: Instance): Unit =
    var currentPosition: Option[MapIndex] = Some(ins.player)
    val result = loop(ins.map)(ins.player, ins.dir)
    println(result)

@main def main() =
    val ins = readFile
    part1(ins)

/*
TODO:
    1. Line Merging
    2. Line Counting
 */
