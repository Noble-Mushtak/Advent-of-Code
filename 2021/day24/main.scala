import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._
object main {
    sealed trait CommandType
    case class AddCmd() extends CommandType {
        override def toString(): String = "+"
    }
    case class SubCmd() extends CommandType {
        override def toString(): String = "-"
    }
    case class MulCmd() extends CommandType {
        override def toString(): String = "*"
    }
    case class DivCmd() extends CommandType {
        override def toString(): String = "/"
    }
    case class ModCmd() extends CommandType {
        override def toString(): String = "%"
    }
    case class EqlCmd() extends CommandType {
        override def toString(): String = "=="
    }
    sealed trait Expression
    case class Inp(val cnt: Int) extends Expression
    case class Variable(val name: String) extends Expression {
        override def toString(): String = name
    }
    case class Number(val value: Int) extends Expression
    case class BinaryExp(val typ: CommandType, val left: Expression, val right: Expression) extends Expression
    sealed trait CommandArg
    case class VariableArg(name: String) extends CommandArg
    case class NumberArg(value: Int) extends CommandArg
    sealed trait Command
    case class InpCmd(name: String) extends Command
    case class BinaryCmd(typ: CommandType, leftVar: String, right: CommandArg) extends Command

    def printExp(exp: Expression): Unit = {
        exp match {
            case Inp(cnt) => print(f"var${cnt}")
            case Number(value) => print(f"${value}")
            case Variable(name) => print(name)
            case BinaryExp(typ, left, right) => {
                print("(")
                printExp(left)
                print(" ")
                print(typ)
                print(" ")
                printExp(right)
                print(")")
            }
        }
    }

    def parseType(word: String): CommandType = {
        if (word == "add") AddCmd()
        else if (word == "sub") SubCmd()
        else if (word == "mul") MulCmd()
        else if (word == "div") DivCmd()
        else if (word == "mod") ModCmd()
        else if (word == "eql") EqlCmd()
        else throw new IllegalArgumentException(f"""command "${word}" not recognized!""")
    }

    def parseArg(word: String): CommandArg = {
        try NumberArg(word.toInt)
        catch {
            case _: NumberFormatException => VariableArg(word)
        }
    }

    def parseLine(line: String): Command = {
        val words = line.split(" ")
        if (words.length == 2) InpCmd(words(1))
        else BinaryCmd(parseType(words(0)), words(1), parseArg(words(2)))
    }

    def initEnv(): Map[String, Expression] = {
        Map("w" -> Variable("w"), "x" -> Variable("x"), "y" -> Variable("y"), "z" -> Variable("z"))
    }

    def getLeft(e: Expression): Expression = {
        e match {
            case BinaryExp(_, left, _) => left
            case _ => throw new IllegalArgumentException("non-binary expression ${e}")
        }
    }

    def getRight(e: Expression): Expression = {
        e match {
            case BinaryExp(_, _, right) => right
            case _ => throw new IllegalArgumentException("non-binary expression ${e}")
        }
    }

    def getValue(e: Expression): Int = {
        e match {
            case Number(value) => value
            case _ => throw new IllegalArgumentException("non-value expression ${e}")
        }
    }

    def processInput(varPairs: Map[Int, (Int, Int)], curStack: Stack[(Int, Int)], cnt: Int, e: Expression): Unit = {
        getLeft(getLeft(e)) match {
            case Variable(_) => {
                curStack.push((cnt, getValue(getRight(getLeft(getRight(e))))))
            }
            case BinaryExp(_, _, _) => {
                val offset = getValue(getRight(getLeft(getLeft(getRight(getLeft(getRight(getLeft(e))))))))
                val (topVar, topOffset) = curStack.pop()
                val curOffset = topOffset+offset
                varPairs += (cnt -> (topVar, curOffset))
                varPairs += (topVar -> (cnt, -curOffset))
            }
            case _ => throw new IllegalArgumentException("unexpected expression ${e}")
        }
    }
    
    def main(args: Array[String]): Unit = {
        val puzzleInput = Source.fromFile(args(0)).getLines().map(parseLine).toVector
        var varPairs = Map[Int, (Int, Int)]()
        var curStack = Stack[(Int, Int)]()
        var env = initEnv()
        var inputCounter = 0
        var commCounter = 0
        for (comm <- puzzleInput) {
            comm match {
                case InpCmd(name) => {
                    if (inputCounter > 0) {
                        processInput(varPairs, curStack, inputCounter, env("z"))
                        env = initEnv()
                    }
                    env(name) = Inp(inputCounter)
                    inputCounter += 1
                }
                case BinaryCmd(AddCmd(), leftVar, NumberArg(0)) => ()
                case BinaryCmd(SubCmd(), leftVar, NumberArg(0)) => ()
                case BinaryCmd(MulCmd(), leftVar, NumberArg(0)) => {
                    env(leftVar) = Number(0)
                }
                case BinaryCmd(MulCmd(), leftVar, NumberArg(1)) => ()
                case BinaryCmd(DivCmd(), leftVar, NumberArg(1)) => ()
                case BinaryCmd(typ, leftVar, right) => {
                    val leftExp = env(leftVar)
                    val rightExp = right match {
                        case VariableArg(name) => env(name)
                        case NumberArg(value) => Number(value)
                    }
                    env(leftVar) = BinaryExp(typ, leftExp, rightExp)
                }
            }
        }
        processInput(varPairs, curStack, inputCounter, env("z"))
        assert(curStack.isEmpty)

        print("Part 1: ")
        for (i <- Range(1, inputCounter+1)) {
             print(9+Math.min(0, varPairs(i)._2))
        }
        println()
        print("Part 2: ")
        for (i <- Range(1, inputCounter+1)) {
             print(1+Math.max(0, varPairs(i)._2))
        }
        println()
    }
}