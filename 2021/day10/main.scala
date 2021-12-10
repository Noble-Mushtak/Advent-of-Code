import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {
    sealed trait ChunkChar
    case class Start(id: Long) extends ChunkChar
    case class End(id: Long) extends ChunkChar

    sealed trait ChunkState
    case class Incomplete(st: Stack[Long]) extends ChunkState
    case class Corrupted(exp: Option[Long], found: Long) extends ChunkState

    def parseChar(ch: Char): ChunkChar = {
        ch match {
            case '(' => Start(0)
            case ')' => End(0)
            case '[' => Start(1)
            case ']' => End(1)
            case '{' => Start(2)
            case '}' => End(2)
            case '<' => Start(3)
            case '>' => End(3)
            case _ => throw new IllegalArgumentException(f"Character '${ch}' not recognized!")
        }
    }

    def calcState(line: Vector[ChunkChar]): ChunkState = {
        var curStack = new Stack[Long]()
        for (ch <- line) {
            ch match {
                case Start(id) => curStack.push(id)
                case End(id) => {
                    curStack.removeHeadOption() match {
                        case None => return Corrupted(None, id)
                        case Some(stTop) => {
                            if (stTop != id) {
                                return Corrupted(Some(stTop), id)
                            }   
                        }
                    }
                }
            }
        }
        Incomplete(curStack)
    }

    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(_.toVector.map(parseChar)).toVector.map(calcState)
        val corruptedVals = Vector(3L, 57L, 1197L, 25137L)
        var part1Ans = 0L
        for (st <- puzzleInput) {
            st match {
                case Corrupted(_, id) => part1Ans += corruptedVals(id.toInt)
                case _ => ()
            }
        }
        println(f"Part 1: ${part1Ans}")

        var incompScores = new ArrayBuffer[Long]()
        for (st <- puzzleInput) {
            st match {
                case Incomplete(st) => {
                    var score = 0L
                    for (num <- st) {
                        score = 5L*score+(num+1)
                    }
                    incompScores += score
                }
                case _ => ()
            }
        }
        val sortedScores = incompScores.sorted
        val part2Ans = sortedScores(sortedScores.length >> 1)
        println(f"Part 2: ${part2Ans}")
    }
}