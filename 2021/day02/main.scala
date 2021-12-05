import scala.io.Source
object main {
    sealed trait Command
    case class Forward(x: Long) extends Command
    case class Down(x: Long) extends Command
    case class Up(x: Long) extends Command

    def parseLine(line: String): Command = {
        val words = line.split(" ")
        val numUnits = words(1).toLong
        words(0) match {
            case "forward" => Forward(numUnits)
            case "down" => Down(numUnits)
            case "up" => Up(numUnits)
            case unrecognized => throw new IllegalArgumentException(f""""${unrecognized}" not recognized""")
        }
    }

    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(parseLine).toList
        var horizLoc = 0L
        var depth = 0L
        for (comm <- puzzleInput) {
            comm match {
                case Forward(numUnits) => horizLoc += numUnits
                case Down(numUnits) => depth += numUnits
                case Up(numUnits) => depth -= numUnits
            }
        }
        System.out.println(f"Part 1: ${horizLoc*depth}")

        var horizLoc2 = 0L
        var aim = 0L
        var depth2 = 0L
        for (comm <- puzzleInput) {
            comm match {
                case Forward(numUnits) => {
                    horizLoc2 += numUnits
                    depth2 += numUnits*aim
                }
                case Down(numUnits) => aim += numUnits
                case Up(numUnits) => aim -= numUnits
            }
        }
        System.out.println(f"Part 2: ${horizLoc2*depth2}")
    }
}