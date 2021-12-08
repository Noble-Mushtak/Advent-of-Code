import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
object main {
    class Entry(val signalPatterns: Vector[Set[Char]], val outputPatterns: Vector[Set[Char]])

    def parseLine(line: String): Entry = {
        val s"$signals | $outputs" = line
        new Entry(signals.split(" ").map(_.toSet).toVector, outputs.split(" ").map(_.toSet).toVector)
    }

    def findByCount(patterns: Vector[Set[Char]], cnt: Int): Set[Char] = {
        patterns.find((pattern) => (pattern.size == cnt))
                .get
    }

    def findByCountAndIntersection(patterns: Vector[Set[Char]], cnt: Int, desPattern: Set[Char], cnt2: Int): Set[Char] = {
        patterns.find((pattern) => (pattern.size == cnt) && (pattern.intersect(desPattern).size == cnt2))
                .get
    }

    def findByCountAndIntersectionAndNot(patterns: Vector[Set[Char]], cnt: Int, desPattern: Set[Char],
                                         cnt2: Int, undesPatterns: Set[Set[Char]]): Set[Char] = {
        patterns.find((pattern) => (pattern.size == cnt) && (pattern.intersect(desPattern).size == cnt2)
                                   && !undesPatterns.contains(pattern))
                .get
    }

    def outputVal(entry: Entry): Long = {
        val patterns = entry.signalPatterns
        val oneSet = findByCount(patterns, 2)
        val fourSet = findByCount(patterns, 4)
        val sevenSet = findByCount(patterns, 3)
        val eightSet = findByCount(patterns, 7)
        val threeSet = findByCountAndIntersection(patterns, 5, oneSet, 2)
        val nineSet = findByCountAndIntersection(patterns, 6, fourSet, 4)
        val zeroSet = findByCountAndIntersectionAndNot(patterns, 6, sevenSet, 3, Set(nineSet))
        val sixSet = findByCountAndIntersection(patterns, 6, sevenSet, 2)
        val fiveSet = findByCountAndIntersection(patterns, 5, sixSet, 5)
        val twoSet = findByCountAndIntersectionAndNot(patterns, 5, sixSet, 4, Set(threeSet))
        val setMap = Map(
            zeroSet -> 0, oneSet -> 1, twoSet -> 2, threeSet -> 3, fourSet -> 4,
            fiveSet -> 5, sixSet -> 6, sevenSet -> 7, eightSet -> 8, nineSet -> 9)

        var answer = 0
        for (pattern <- entry.outputPatterns) {
            answer *= 10
            answer += setMap(pattern)
        }
        answer
    }

    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(parseLine).toVector
        val desCounts = Set(2, 3, 4, 7)
        var part1Ans = 0
        for (entry <- puzzleInput) {
            for (pattern <- entry.outputPatterns) {
                if (desCounts.contains(pattern.size)) {
                    part1Ans += 1
                }
            }
        }
        println(f"Part 1: ${part1Ans}")

        println(f"Part 2: ${puzzleInput.map(outputVal).sum}")
    }
}