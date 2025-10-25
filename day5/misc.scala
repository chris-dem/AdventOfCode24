package misc

import scala.util.matching.Regex
import scala.collection.immutable.BitSet

type SolMap = Map[Int, BitSet]
type Bool = Boolean

val reg: Regex = """(\d+)\|(\d+)""".r

def verifyUpdate(map: SolMap)(update: List[Int]): Bool =
    var initList = BitSet((0 until 100)*)
    val table = update.foldLeft[Option[BitSet]](Some(initList))((lis, x) =>
        lis match {
            case None => None
            case Some(m) if (!m.contains(x)) => {
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
    table.isDefined

def getMiddleValue(update: List[Int]): Int = update(update.length / 2)
