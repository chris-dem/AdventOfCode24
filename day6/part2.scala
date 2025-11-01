package partB

import scala.io.Source
import scala.collection.immutable.TreeSet
import scala.compiletime.ops.boolean
import scala.collection.Searching._
import scala.annotation.tailrec
import java.beans.Introspector
import java.time.Year

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
            case Direction.Left  => MapIndex(mapIndex.x, getCols - 1)
            case Direction.Right => MapIndex(mapIndex.x, 0)

    def convertIndex(ind: Int): MapIndex =
        val cols = getCols
        MapIndex(ind / cols, ind % cols)
}

class GridStruct(
    val rowMap: Vector[TreeSet[Int]],
    val colMap: Vector[TreeSet[Int]],
    val startPos: MapIndex
) {
    def getClosest(indx: MapIndex, direction: Direction): Option[Int] =
        direction match {
            case Direction.Up =>
                colMap(indx.y)
                    .from(indx.x)
                    .headOption
                    .map(_ - 1)
            case Direction.Down =>
                colMap(indx.y)
                    .to(indx.x)
                    .lastOption
                    .map(_ + 1)
            case Direction.Right =>
                rowMap(indx.x)
                    .to(indx.y)
                    .lastOption
                    .map(_ + 1)
            case Direction.Left =>
                rowMap(indx.x)
                    .from(indx.y)
                    .headOption
                    .map(_ - 1)
        }
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
          colMap.updated(other.y, colMap(other.y) + other.x),
          startPos
        )
}

object GridStruct {
    def init(startPos: MapIndex) = GridStruct(
      rowMap = Vector.fill(MapDims.getRows)(TreeSet.empty),
      colMap = Vector.fill(MapDims.getCols)(TreeSet.empty),
      startPos = startPos
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
        .foldLeft(GridStruct.init(explorerIndex._2))(_ + _)
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

    def toPoints: Iterator[MapIndex] =
        if isHorizontal then
            (pointA.y to pointB.y).map(MapIndex(pointA.x, _)).toIterator
        else (pointA.x to pointB.x).map(MapIndex(_, pointA.y)).toIterator

    def isHorizontal = dir == Direction.Right || dir == Direction.Left
    def <&>(other: Line): Boolean =
        if (dir.turn != other.dir) {
            return false
        }
        dir match {
            case Direction.Up =>
                (pointA.y >= other.pointA.y) && pointA.x <= other.pointA.x && other.pointA.x <= pointB.x
            case Direction.Down =>
                (pointA.y <= other.pointA.y) && pointA.x <= other.pointA.x && other.pointA.x <= pointB.x
            case Direction.Left =>
                (pointA.x <= other.pointA.x) && pointA.y <= other.pointA.y && other.pointA.y <= pointB.y
            case Direction.Right =>
                (pointA.x >= other.pointA.x) && pointA.y <= other.pointA.y && other.pointA.y <= pointB.y
        }

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

case class Interval(val fst: Int, val snd: Int)
object Interval {
    implicit val ord: Ordering[Interval] = new Ordering[Interval] {
        def compare(x: Interval, y: Interval): Int =
            (x, y) match {
                case (Interval(_, a), Interval(b, _)) if a < b            => -1
                case (Interval(b, _), Interval(_, a)) if a < b            => 1
                case (Interval(a, b), Interval(c, d)) if a <= c && d <= b => 0
                case (Interval(a, b), Interval(c, d)) if c <= a && b <= d => 0
                case _                                                    => ???
            }
    }
}

case class IntervalVec(val vec: Vector[Interval]) {
    override def toString(): String = s"[${vec.mkString(", ")}]"
    private def binIns(p: Interval) =
        vec.search(p)
            .match
                case Found(_) => {
                    println(List(vec, p))
                    ???
                }
                case InsertionPoint(insertionPoint) =>
                    val (left, right) = vec.splitAt(insertionPoint)
                    left ++ Vector(p) ++ right

    def findInt(p: Int) = findPoint(Interval(p, p))
    def findPoint(p: Interval) =
        vec.search(p).match {
            case Found(foundIndex) => Option(foundIndex)
            case _                 => None
        }

    def add(int: Interval) =
        assert(
          int.fst <= int.snd,
          s"${int.fst} should be greater than ${int.snd}"
        )
        IntervalVec(binIns(int))
    def +(other: (Int, Int)) = this.add(Interval(other._1, other._2))

    def from(p: Int): Vector[Interval] =
        vec.search(Interval(p, p)).match {
            case Found(i) => vec.drop(i)
            case InsertionPoint(insertionPoint) =>
                vec.drop(insertionPoint)
        }

    def to(p: Int): Vector[Interval] =
        // println(List("WTF",vec, p, vec.search(Interval(p, p))))
        vec.search(Interval(p, p)).match {
            case Found(i) => vec.take(i + 1)
            case InsertionPoint(insertionPoint) =>
                vec.take(insertionPoint)
        }

}
case class LineStruct(
    val up: Vector[IntervalVec],
    val down: Vector[IntervalVec],
    val left: Vector[IntervalVec],
    val right: Vector[IntervalVec]
) {
    override def toString(): String =
        s"""
        LineStruct:
            -up : ${up}
            -down : ${down}
            -right : ${right}
            -left : ${left}
        """
    def addLine(line: Line): LineStruct =
        line.dir match {
            case Direction.Up =>
                LineStruct(
                  up.updated(
                    line.pointA.y,
                    up(line.pointA.y) + (line.pointA.x, line.pointB.x)
                  ),
                  down,
                  left,
                  right
                )
            case Direction.Down =>
                LineStruct(
                  up,
                  down.updated(
                    line.pointA.y,
                    down(line.pointA.y) + (line.pointA.x, line.pointB.x)
                  ),
                  left,
                  right
                )
            case Direction.Right =>
                LineStruct(
                  up,
                  down,
                  left,
                  right.updated(
                    line.pointA.x,
                    right(line.pointB.x) + (line.pointA.y, line.pointB.y)
                  )
                )
            case Direction.Left =>
                LineStruct(
                  up,
                  down,
                  left.updated(
                    line.pointA.x,
                    left(line.pointA.x) + (line.pointA.y, line.pointB.y)
                  ),
                  right
                )
        }

    def |+|(line: Line) = this.addLine(line)

    def addObstacle(
        vec: Vector[IntervalVec],
        inverseDir: Boolean
    )(ind: Int, el: Int) =
        vec(ind)
            .findInt(el)
            .match
                case Some(int) => {
                    val Interval(a, b) = vec(ind).vec(int)
                    val t = vec.updated(
                      ind,
                      IntervalVec(vec(ind).vec.patch(int, None, 1))
                    ) // Remove
                    val ret =
                        if inverseDir && el > a + 1 then
                            t.updated(ind, t(ind).add(Interval(a, el - 1)))
                        else if !inverseDir && el < b - 1 then
                            t.updated(ind, t(ind).add(Interval(el + 1, b)))
                        else t
                    Some(ret)
                }
                case None => None

    def insertObs(obs: MapIndex): Option[LineStruct] =
        var count = 0
        val newUp = {
            addObstacle(up, false)(obs.y, obs.x).match
                case Some(u) => {
                    count += 1
                    u
                }
                case None => up
        }
        val newDown = {
            addObstacle(down, true)(obs.y, obs.x).match
                case Some(u) => {
                    count += 1
                    u
                }
                case None => down
        }

        val newLeft = {
            addObstacle(left, false)(obs.x, obs.y).match
                case Some(u) => {
                    count += 1
                    u
                }
                case None => left
        }

        val newRight = {
            addObstacle(right, true)(obs.x, obs.y).match
                case Some(u) => {
                    count += 1
                    u
                }
                case None => right
        }
        if count > 1 then {
            None
        } else {
            Option(
              LineStruct(
                up = newUp,
                down = newDown,
                right = newRight,
                left = newLeft
              )
            )
        }

    def getClosest(point: MapIndex, dir: Direction): Option[Int] =
        dir.match
            case Direction.Up =>
                up(point.y)
                    .from(point.x)
                    .headOption
                    .map((_.fst))
            case Direction.Down =>
                down(point.y).to(point.x).lastOption.map(_.snd)
            case partB.Direction.Left =>
                left(point.x).from(point.y).headOption.map(_.fst)
            case partB.Direction.Right =>
                right(point.x).to(point.y).lastOption.map(_.snd)

}

object LineStruct {
    def createStruct = LineStruct(
      up = Vector.fill(MapDims.getCols)(IntervalVec(Vector.empty)),
      down = Vector.fill(MapDims.getCols)(IntervalVec(Vector.empty)),
      left = Vector.fill(MapDims.getRows)(IntervalVec(Vector.empty)),
      right = Vector.fill(MapDims.getRows)(IntervalVec(Vector.empty))
    )
}

def log[A](x: A) =
    println(x)
    x

@tailrec
def checkPoint(grid: GridStruct, lineStruct: LineStruct, dir: Direction)(
    p: MapIndex
): Boolean =
    val gridPoint = grid.getClosest(p, dir)
    val linePoint = lineStruct.getClosest(p, dir)
    val minPoint =
        if (dir == Direction.Up || dir == Direction.Left) then 1 else -1
    // println(List(p, dir, gridPoint, linePoint))
    (gridPoint, linePoint) match
        case (None, None) => false // Out of bounds
        case (b, Some(a)) if b == None || (b.get - a) * minPoint > 0 =>
            true // Getting into a different path
        case (a, b) => {
            // println(List(a, b, c, lineStruct))
            val pObs = a.get
            val newDir = dir.turn
            val endPoint =
                if dir == Direction.Up || dir == Direction.Down then {
                    MapIndex(pObs, p.y)
                } else {
                    MapIndex(p.x, pObs)
                }
            // println(List(p, endPoint))
            val newLine = if (endPoint != p) {
                lineStruct |+| Line.createLine(p, endPoint).get
            } else lineStruct
            checkPoint(grid, newLine, newDir)(endPoint)
        }

def findPoints(grid: GridStruct, lineStruct: LineStruct, line: Line): Int =
    val points = line.toPoints
    val ps = points
        .filter(x =>
            val p = x + line.dir
            (0 `until` MapDims.getCols)
                .contains(p.y) &&
            (0 `until` MapDims.getRows).contains(p.x)
            && !grid.colMap(p.y).contains(p.x)
            && grid.startPos != p
        ) // If it already exists
        .filter(x => {
            // print(s"===For Line ${line}==")
            fixGrids(grid, lineStruct, x + line.dir) match {
                case None                => false
                case Some(lineADT, grid) =>
                    // log(
                    checkPoint(grid, lineADT, line.dir.turn)(x)
                // )
            }

        })
        .toList
    // println(ps)
    ps.length

@tailrec
def part2Loop(
    grid: GridStruct,
    lineStruct: LineStruct = LineStruct.createStruct
)(
    root: List[Line],
    count: Int = 0
): Int =
    root match
        case Nil => count
        case head :: next => {
            val newStruct = lineStruct |+| head
            part2Loop(grid, newStruct)(
              next,
              findPoints(grid, newStruct, head) + count
            )
        }

def fixGrids(grid: GridStruct, lineADT: LineStruct, obs: MapIndex) =
    val newGrid = grid + obs
    lineADT.insertObs(obs).map((_, newGrid))

def part2(ins: Instance): Unit =
    var currentPosition: Option[MapIndex] = Some(ins.player)
    val result = loop(ins.map)(ins.player, ins.dir).toList
    // println(result)
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
    println(part2Loop(ins.map)(lines))

@main def main(fileName: String) =
    val ins = readFile(fileName)
    part2(ins)

/* Reachable lines
1. Inclusion checks on intersectins

 */
