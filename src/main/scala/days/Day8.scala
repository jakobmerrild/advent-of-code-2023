package days

import scala.annotation.tailrec
import scala.util.matching.Regex

class Day8(input: String) extends Puzzle {

  val instructions: List[Instruction] =
    input.split("\n")(0).map(Instruction.apply).toList

  val numberInstructions = instructions.size

  val nodes: Map[String, Node] =
    input.split("\n").drop(2).view.map(Node.apply).map(n => n.id -> n).toMap

  def followInstruction(id: String, inst: Instruction): String =
    val node = nodes(id)
    inst match
      case Instruction.Left  => node.left
      case Instruction.Right => node.right

  def finalNode(startNode: String): String =
    instructions.foldLeft(startNode) { followInstruction }

  val finalNodes = nodes.keys.map(id => id -> finalNode(id)).toMap

  @tailrec
  private def countSteps(
      startNode: String,
      isFinalNode: String => Boolean,
      acc: Long
  ): Long =
    if(isFinalNode(startNode))
      acc
    else
      countSteps(finalNodes(startNode), isFinalNode, acc + 1L)

  override def solve: String = (countSteps("AAA", _ == "ZZZ", 0L) * numberInstructions).toString

  override def solve2: String =
    (nodes.keys.filter(_.endsWith("A"))
      .map(countSteps(_, _.endsWith("Z"), 0L))
      .product * numberInstructions)
      .toString
}

final case class Node(id: String, left: String, right: String)

object Node {
  private val nodeRegex: Regex = """^(\w{3}) = \((\w{3}), (\w{3})\)$""".r
  def apply(s: String): Node =
    s match
      case nodeRegex(id, left, right) => Node(id, left, right)
      case _ =>
        throw new IllegalArgumentException(s"'s' not a valid Node string: $s")
}

sealed trait Instruction

object Instruction {
  case object Left extends Instruction
  case object Right extends Instruction

  def apply(c: Char): Instruction =
    c match
      case 'L' => Left
      case 'R' => Right
      case _ =>
        throw new IllegalArgumentException(s"'c' not a valid instruction: $c")
}
