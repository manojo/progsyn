package graph

abstract class Node[T] {
  var parent: Option[Node[T]]
  /**
   * Indicates, from the left, which child of its parent this node is
   * zero-based indexing applies
   */
  val pos: Int
}

object Node {
  def copy[T](n: Node[T], p: Option[Node[T]], pos: Int) = n match {
    case a @ AndNode(_, _, _, _) => a.copy(parent = p, pos = pos)
    case o @ OrNode(_, _, _)     => o.copy(parent = p, pos = pos)
    case t @ Terminal(_, _, _)   => t.copy(parent = p, pos = pos)
  }
}

case class AndNode[T](
    var parent: Option[Node[T]],
    pos: Int,
    children: Array[Node[T]],
    builder: List[T] => T) extends Node[T] {

  import scala.collection.mutable.ListBuffer
  val solutions: Array[ListBuffer[T]] = children.map(_ => ListBuffer.empty[T])
}

case class OrNode[T](
  var parent: Option[Node[T]],
  pos: Int,
  children: Array[Node[T]]) extends Node[T]

case class Terminal[T](
  value: T,
  var parent: Option[Node[T]],
  pos: Int) extends Node[T]
