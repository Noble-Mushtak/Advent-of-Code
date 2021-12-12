import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.util.control.Breaks._
object main {
    val STARTV = "start"
    val GOALV = "end"
    
    class Edge(val u: String, val v: String)
    def parseLine(line: String): Edge = {
        val s"$u-$v" = line
        new Edge(u, v)
    }

    def addEdge(edges: Map[String, ArrayBuffer[String]], u: String, v: String): Unit = {
        if (edges.contains(u)) {
            edges(u) += v
        } else {
            edges += (u -> ArrayBuffer(v))
        }
    }

    def countPaths(edges: Map[String, ArrayBuffer[String]], prevCaves: Set[String], brokeRules: Boolean, start: String, goal: String): Long = {
        if (start == goal) {
            1
        } else {
            val newBrokeRules = if ((start == start.toLowerCase()) && prevCaves.contains(start)) {
                if (brokeRules || (start == STARTV)) return 0
                else true
            } else brokeRules
            val newCaves = prevCaves+start
            edges(start).map(neighbor => countPaths(edges, newCaves, newBrokeRules, neighbor, goal)).sum
        }
    }

    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(parseLine).toVector
        val edges = Map[String, ArrayBuffer[String]]()
        for (edg <- puzzleInput) {
            addEdge(edges, edg.u, edg.v)
            addEdge(edges, edg.v, edg.u)
        }
        val part1Ans = countPaths(edges, Set(), true, STARTV, GOALV)
        println(f"Part 1: ${part1Ans}")
        val part2Ans = countPaths(edges, Set(), false, STARTV, GOALV)
        println(f"Part 2: ${part2Ans}")
    }
}