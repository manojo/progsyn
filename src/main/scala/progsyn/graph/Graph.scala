package progsyn.graph

import scala.reflect.ClassTag

abstract class Node[T: ClassTag] {
  var parent: Option[Node[T]]
  /**
   * Indicates, from the left, which child of its parent this node is
   * zero-based indexing applies
   */
  val pos: Int
  val children: Array[Node[T]]
}

object Node {
  def copy[T: ClassTag](n: Node[T], p: Option[Node[T]], pos: Int) = n match {

    /**
     * we don't need to clear the `solutions` of an AndNode because
     * we only use copy in a top-down flow, i.e. `solutions` will be
     * empty
     */
    case a @ AndNode(_, _, _, _) => a.copy(parent = p, pos = pos)
    case o @ OrNode(_, _, _)     => o.copy(parent = p, pos = pos)
    case t @ Terminal(_, _, _)   => t.copy(parent = p, pos = pos)
  }
}

case class AndNode[T: ClassTag](
    var parent: Option[Node[T]],
    pos: Int,
    children: Array[Node[T]],
    builder: List[T] => T) extends Node[T] {

  import scala.collection.mutable.ListBuffer
  val solutions: Array[ListBuffer[T]] = children.map(_ => ListBuffer.empty[T])
}

case class OrNode[T: ClassTag](
  var parent: Option[Node[T]],
  pos: Int,
  children: Array[Node[T]]) extends Node[T]

case class Terminal[T: ClassTag](
    values: Stream[T],
    var parent: Option[Node[T]],
    pos: Int) extends Node[T] {

  val children = Array.empty[Node[T]]
}
