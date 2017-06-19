package graph

/**
 * A datatype that specifies which
 * direction a given node has come from
 */
abstract class Direction
case object Left extends Direction
case object Right extends Direction
case object Down extends Direction

abstract class Node[T] {
  var parent: Option[Node[T]]
  /** Indicates if this is the left, right or down child of the parent */
  val dir: Direction
}

object Node {
  def copy[T](n: Node[T], p: Option[Node[T]], d: Direction) = n match {
    case a @ AndNode(_, _, _, _, _) => a.copy(parent = p, dir = d)
    case o @ OrNode(_, _, _, _)     => o.copy(parent = p, dir = d)
    case t @ Terminal(_, _, _)      => t.copy(parent = p, dir = d)
  }
}

case class AndNode[T](
    var parent: Option[Node[T]],
    dir: Direction,
    leftChild: Node[T],
    rightChild: Node[T],
    builder: (T, T) => T) extends Node[T] {

  import scala.collection.mutable.ListBuffer
  val leftSolutions: ListBuffer[T] = ListBuffer.empty[T]
  val rightSolutions: ListBuffer[T] = ListBuffer.empty[T]
}

case class OrNode[T](
  var parent: Option[Node[T]],
  dir: Direction,
  var leftChild: Node[T],
  rightChild: Node[T]) extends Node[T]

case class Terminal[T](
  value: T,
  var parent: Option[Node[T]],
  dir: Direction) extends Node[T]
