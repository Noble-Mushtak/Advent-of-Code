import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._

object main {
    sealed trait SnailNumber
    case class RegularNumber(num: Int) extends SnailNumber {
        override def toString(): String = num.toString
    }
    case class PairNumber(left: SnailNumber, right: SnailNumber) extends SnailNumber {
        override def toString(): String = s"[$left,$right]"
    }

    def parseSnailNumberHelp(string: String, idx: Int): (SnailNumber, Int) = {
        string(idx) match {
            case '[' => {
                val (num1, idx1) = parseSnailNumberHelp(string, idx+1)
                assert(string(idx1) == ',')
                val (num2, idx2) = parseSnailNumberHelp(string, idx1+1)
                assert(string(idx2) == ']')
                (PairNumber(num1, num2), idx2+1)
            }
            case _ => {
                var idx1 = idx
                while ((idx1 < string.length) && (string(idx1) != ',') && (string(idx1) != ']')) {
                    idx1 += 1
                }
                (RegularNumber(string.substring(idx, idx1).toInt), idx1)
            }
        }
    }

    def parseSnailNumber(string: String): SnailNumber = {
        val (num, idx) = parseSnailNumberHelp(string, 0)
        if (idx != string.length) {
            throw new IllegalArgumentException(s"Found content after snail number: ${string.substring(idx, string.length)}")
        }
        num
    }

    def incLeftmostBy(num: SnailNumber, inc: Int): SnailNumber = {
        num match {
            case RegularNumber(r) => RegularNumber(r+inc)
            case PairNumber(num1, num2) => PairNumber(incLeftmostBy(num1, inc), num2)
        }
    }

    def incRightmostBy(num: SnailNumber, inc: Int): SnailNumber = {
        num match {
            case RegularNumber(r) => RegularNumber(r+inc)
            case PairNumber(num1, num2) => PairNumber(num1, incRightmostBy(num2, inc))
        }
    }

    def handleExplodeHelp(num1: SnailNumber, num2: SnailNumber, depth: Int): Option[(Option[Int], Option[Int], SnailNumber)] = {
        if (depth == 4) {
            val (val1: Int, val2: Int) = (num1, num2) match {
                case (RegularNumber(r1), RegularNumber(r2)) => (r1, r2)
                case _ => new IllegalArgumentException("Pair nested too deeply!")
            }
            return Some((Some(val1), Some(val2), RegularNumber(0)))
        }
        
        val res1 = num1 match {
            case RegularNumber(_) => None
            case PairNumber(num11, num12) => handleExplodeHelp(num11, num12, depth+1)
        }
        res1 match {
            case None => {
                val res2 = num2 match {
                    case RegularNumber(_) => None
                    case PairNumber(num21, num22) => handleExplodeHelp(num21, num22, depth+1)
                }
                res2 map { case (leftOpt, rightOpt, newSn) => {
                    val newSnLeft = leftOpt match {
                        case None => num1
                        case Some(leftInc) => incRightmostBy(num1, leftInc)
                    }
                    (None, rightOpt, PairNumber(newSnLeft, newSn))
                } }
            }
            case Some((leftOpt, rightOpt, newSn)) => {
                val newSnRight = rightOpt match {
                    case None => num2
                    case Some(rightInc) => incLeftmostBy(num2, rightInc)
                }
                Some((leftOpt, None, PairNumber(newSn, newSnRight)))
            }
        }
    }

    def handleExplode(num: SnailNumber): Option[SnailNumber] = {
        num match {
            case PairNumber(num1, num2) => {
                handleExplodeHelp(num1, num2, 0) map { case (_, _, newSn) => newSn }
            }
            case _ => None
        }
    }

    def handleSplit(num: SnailNumber): Option[SnailNumber] = {
        num match {
            case RegularNumber(r) => {
                if (r >= 10) Some(PairNumber(RegularNumber(r/2), RegularNumber((r+1)/2)))
                else None
            }
            case PairNumber(num1, num2) => {
                handleSplit(num1) match {
                    case Some(newSn) => Some(PairNumber(newSn, num2))
                    case None => {
                        handleSplit(num2) map { PairNumber(num1, _) }
                    }
                }
            }
        }
    }

    def addSnailNumbers(num1: SnailNumber, num2: SnailNumber): SnailNumber = {
        var res: SnailNumber = PairNumber(num1, num2)
        breakable { while (true) {
            res = handleExplode(res).getOrElse(handleSplit(res).getOrElse(break()))
        } }
        res
    }

    def calcMagnitude(num: SnailNumber): Int = {
        num match {
            case RegularNumber(r) => r
            case PairNumber(num1, num2) => 3*calcMagnitude(num1)+2*calcMagnitude(num2)
        }
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(parseSnailNumber).toVector
        var res = puzzleInput(0)
        for (i <- Range(1, puzzleInput.length)) {
            res = addSnailNumbers(res, puzzleInput(i))
        }
        println(f"Part 1: ${calcMagnitude(res)}")

        var maxMag = 0
        for (i <- Range(0, puzzleInput.length)) {
            for (j <- Range(0, puzzleInput.length)) {
                if (i != j) {
                    maxMag = Math.max(maxMag, calcMagnitude(addSnailNumbers(puzzleInput(i), puzzleInput(j))))
                }
            }
        }
        println(f"Part 2: ${maxMag}")
    }
}