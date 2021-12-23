import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {    
    def base5ToInt(repr: Array[Int]): Long = {
        repr.foldLeft(0L)((res, num) => 5L*res+num.toLong)
    }

    def longPow(base: Long, exp: Long): Long = {
        var curExp = exp
        var curPow = base
        var ans = 1L
        while (curExp > 0) {
            if ((curExp & 1) == 1) ans *= curPow
            curPow *= curPow
            curExp >>= 1
        }
        ans
    }

    def getBase5Dig(num: Long, idx: Long): Int = {
        ((num/longPow(5, idx)) % 5).toInt
    }
    
    def formatDig(dig: Int): Char = {
        if (dig == 0) '.'
        else (dig-1+'A'.toInt).toChar
    }

    def formatNode(numRooms: Int, numSpacesPerRoom: Int, node: Long): String = {
        var ans = ""
        for (i <- Range(0, 2*numRooms+5)) ans += "#"
        ans += "\n"

        ans += "#"
        ans += formatDig(getBase5Dig(node, numRooms*numSpacesPerRoom))
        for (j <- Range(0, numRooms+1)) {
            if (j > 0) ans += "."
            ans += formatDig(getBase5Dig(node, numRooms*numSpacesPerRoom+j+1))
        }
        ans += formatDig(getBase5Dig(node, numRooms*numSpacesPerRoom+numRooms+2))
        ans += "#\n"

        for (i <- Range(0, numSpacesPerRoom)) {
            if (i == 0) ans += "##"
            else ans += "  "
            for (j <- Range(0, numRooms)) {
                ans += "#"
                ans += formatDig(getBase5Dig(node, numSpacesPerRoom*j+i))
            }
            ans += "#"
            if (i == 0) ans += "##"
            ans += "\n"
        }

        ans += "  "
        for (i <- Range(0, 2*numRooms+1)) ans += "#"
        ans
    }

    def addVertex(dijkQ: PriorityQueue[(Long, Long)], dsts: Map[Long, Long], prev: Map[Long, Long], curNode: Long, newNode: Long, newCost: Long): Unit = {
        if (!(dsts.contains(newNode)) || (dsts(newNode) > newCost)) {
            prev(newNode) = curNode
            dsts(newNode) = newCost
            dijkQ.enqueue((-newCost, newNode))
        }
    }

    def runDijk(numRooms: Int, numSpacesPerRoom: Int, startNode: Long, endNode: Long, printPath: Boolean): Long = {
        var dijkQ = PriorityQueue((0L, startNode))
        var dsts = Map(startNode -> 0L)
        var prev = Map[Long, Long]() 
        while (!dijkQ.isEmpty) { breakable {
            val (negDst, curNode) = dijkQ.dequeue()
            val curDst = -negDst
            if (curDst != dsts(curNode)) break()
            if (curNode == endNode) {
                if (printPath) {
                    var path = ArrayBuffer[Long]()
                    var pathNode = endNode
                    while (prev.contains(pathNode)) {
                        path += pathNode
                        pathNode = prev(pathNode)
                    }
                    path += pathNode
                    for (i <- Range(0, path.length)) {
                        println(dsts(path(path.length-i-1)))
                        println(formatNode(numRooms, numSpacesPerRoom, path(path.length-i-1)))
                    }
                }
                
                return curDst
            }

            for (i <- Range(0, numRooms)) { breakable {
                var curTup: Option[(Int, Long, Int)] = None
                for (j <- Range(0, numSpacesPerRoom)) { breakable {
                    if (curTup != None) break()
                    val curDig = getBase5Dig(curNode, numSpacesPerRoom*i+j)
                    if (curDig > 0) {
                        curTup = Some((curDig, longPow(5, numSpacesPerRoom*i+j), j+1))
                        break()
                    }
                } }
                val (curDig, curPow, dst) = curTup.getOrElse(break())
                var j = i+1
                while ((j >= 0) && (getBase5Dig(curNode, numRooms*numSpacesPerRoom+j) == 0)) {
                    val newNode = curNode-curDig*curPow+curDig*longPow(5, numRooms*numSpacesPerRoom+j)
                    val movement = dst+1+2*(i+1-j)-(if (j == 0) { 1 } else { 0 })
                    addVertex(dijkQ, dsts, prev, curNode, newNode, curDst+longPow(10, curDig-1)*movement)
                    j -= 1
                }
                j = i+2
                while ((j < 7) && (getBase5Dig(curNode, numRooms*numSpacesPerRoom+j) == 0)) {
                    val newNode = curNode-curDig*curPow+curDig*longPow(5, numRooms*numSpacesPerRoom+j)
                    val movement = dst+1+2*(j-i-2)-(if (j == 6) { 1 } else { 0 })
                    addVertex(dijkQ, dsts, prev, curNode, newNode, curDst+longPow(10, curDig-1)*movement)
                    j += 1
                }
            } }

            for (i <- Range(0, 7)) { breakable {
                val curDig = getBase5Dig(curNode, numRooms*numSpacesPerRoom+i)
                if (curDig == 0) break()
                var curTup: Option[(Int, Int)] = None
                for (j <- Range(0, numSpacesPerRoom)) { breakable {
                    if (curTup != None) break()
                    for (k <- Range(0, j)) {
                        if (getBase5Dig(curNode, numSpacesPerRoom*curDig-k-1) != curDig) {
                            break()
                        }
                    }
                    for (k <- Range(j, numSpacesPerRoom)) {
                        if (getBase5Dig(curNode, numSpacesPerRoom*curDig-k-1) != 0) {
                            break()
                        }
                    }
                    curTup = Some((numSpacesPerRoom-j, numSpacesPerRoom*curDig-j-1))
                    break()
                } }
                val (dst, nextDig) = curTup.getOrElse(break())
                var spaceExists = true
                val curIter = if (i > curDig) { Range(curDig+1, i) } else { Range(i+1, curDig+1) }
                for (j <- curIter) { breakable {
                    if (getBase5Dig(curNode, numRooms*numSpacesPerRoom+j) != 0) {
                        spaceExists = false
                        break()
                    }
                } }
                if (!spaceExists) break()
                val newNode = curNode-curDig*longPow(5, numRooms*numSpacesPerRoom+i)+curDig*longPow(5, nextDig)
                val movement = dst+1+2*(if (i > curDig) { i-curDig-1 } else { curDig-i })-(if ((i == 0) || (i == 6)) { 1 } else { 0 })
                addVertex(dijkQ, dsts, prev, curNode, newNode, curDst+longPow(10, curDig-1)*movement)
            } }
        } }
        throw new IllegalArgumentException("Could not find path from start to end!")
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().toVector
        val printPath = (args.length >= 2) && (args(1) == "-printpath")
        
        val startNode1 = base5ToInt(Array(puzzleInput(3)(9), puzzleInput(2)(9), puzzleInput(3)(7), puzzleInput(2)(7), puzzleInput(3)(5), puzzleInput(2)(5), puzzleInput(3)(3), puzzleInput(2)(3)).map(ch => ch.toInt-'A'.toInt+1).toArray)
        val endNode1 = base5ToInt(Array(4, 4, 3, 3, 2, 2, 1, 1))
        println(f"Part 1: ${runDijk(4, 2, startNode1, endNode1, printPath)}")
        val startNode2 = base5ToInt(Array(puzzleInput(3)(9), 'C', 'A', puzzleInput(2)(9), puzzleInput(3)(7), 'A', 'B', puzzleInput(2)(7), puzzleInput(3)(5), 'B', 'C', puzzleInput(2)(5), puzzleInput(3)(3), 'D', 'D', puzzleInput(2)(3)).map(ch => ch.toInt-'A'.toInt+1).toArray)
        val endNode2 = base5ToInt(Array(4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1))
        println(f"Part 2: ${runDijk(4, 4, startNode2, endNode2, printPath)}")
    }
}