import scala.io.Source
import scala.collection.immutable.TreeSet
import scala.collection.immutable.TreeMap
import scala.util.matching.Regex
import scala.annotation.tailrec

type DataStructure = Vector[TreeSet[MapIndex]]

object MapDims {
    private var rows: Option[Int] = None
    private var cols: Option[Int] = None

    def setDims(rows: Int, cols: Int) = {
        this.rows = Some(rows)
        this.cols = Some(cols)
    }

    def getRows = this.rows.get
    def getCols = this.cols.get

    def inDims(other: MapIndex) =
        0 <= other.row && other.row < this.getRows &&
            0 <= other.col && other.col < this.getCols
}

case class MapIndex(val row: Int, val col: Int) {
    def +(other: (Int, Int)) = MapIndex(row + other._1, col + other._2)

    def getVon: Seq[MapIndex] = {
        List(this + (1, 0), this + (-1, 0), this + (0, 1), this + (0, -1))
            .filter(MapDims.inDims)
    }
}

object MapIndex {
    implicit val byXY: Ordering[MapIndex] = Ordering.by(m => (m.row, m.col))
}

def readFile(fileName: String) = {
    val lines = Source.fromFile(fileName).getLines().toVector
    MapDims.setDims(lines.length, lines(0).length())
    lines.zipWithIndex
        .flatMap((r, i) =>
            r.zipWithIndex
                .filter(_._1 != '.')
                .map((c, j) => (c.toInt - '0'.toInt, MapIndex(i, j)))
        )
        .foldLeft(Vector.fill(10)(TreeSet.empty[MapIndex]))((acc, el) =>
            acc.updated(el._1, acc(el._1) + el._2)
        )

}

def log[A](x: A): A =
    println(x)
    x

def dfs(map: DataStructure, level: Int = 0)(root: MapIndex): TreeSet[MapIndex] =
    level.match {
        case 9 => TreeSet(root)
        case i =>
            map(level + 1)
                .intersect(root.getVon.toSet)
                .view
                .iterator
                .map(dfs(map, level + 1))
                .foldLeft(TreeSet.empty[MapIndex])(_ ++ _)
    }

def solP1(map: DataStructure) =
    map(0).view.iterator.map(dfs(map)).toList.map(_.size).sum

def combineTrees[K](a: TreeMap[K, Int], b: TreeMap[K, Int]) = b.foldLeft(a) {
    case (acc, (key, value)) =>
        acc.updated(key, acc.get(key).map(_ + value).getOrElse(value))
}

// def dfsP2(map: DataStructure, level: Int = 0)(
//     root: MapIndex
// ): TreeMap[MapIndex, Int] =
//     level.match {
//         case 9 => TreeMap.empty + (root -> 1)
//         case i =>
//             map(level + 1)
//                 .intersect(root.getVon.toSet)
//                 .view
//                 .iterator
//                 .map(dfsP2(map, level + 1))
//                 .foldLeft(TreeMap.empty[MapIndex, Int])(combineTrees)
//     }

def dfsP2(map: DataStructure, level: Int = 0)(
    root: MapIndex
): Int =
    level.match {
        case 9 => 1
        case i =>
            map(level + 1)
                .intersect(root.getVon.toSet)
                .view
                .iterator
                .map(dfsP2(map, level + 1))
                .sum
    }

def solP2(map: DataStructure) =
    map(0).view.iterator
        .map(dfsP2(map))
        .sum

@main def main(fileName: String) =
    val ins = readFile(fileName)
    // println(solP1(is))
    println(solP2(ins))
