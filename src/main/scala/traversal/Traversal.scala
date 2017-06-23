package traversal

import graph._
import scala.collection.immutable.Queue

trait Traversal {
  import Node._

  /**
   * The inner loop doing a breadth-first traversal over a node
   * structure
   * Since we are generating a grammar, we will always add new children
   * for every node we see, setting their parent the node we just have observed
   * This will ensure that the right parent is preserved at all stages
   */
  private def loop[T](ls: Queue[Node[T]]): Stream[T] = {
    if (ls.isEmpty) Stream.empty
    else {
      val (hd, tl) = ls.dequeue
      hd match {
        case AndNode(p, _, children, _) =>
          val newChildren = children.zipWithIndex map { case (child, idx) =>
            copy(child, p = Some(hd), pos = idx)
          }
          loop(tl ++ newChildren)

        case OrNode(p, _, children)  =>
          val newChildren = children.zipWithIndex map { case (child, idx) =>
            copy(child, p = Some(hd), pos = idx)
          }
          loop(tl ++ newChildren)

        case Terminal(v, p, pos)  =>
          backPropagate(p, v #:: Stream.empty, tl, pos)
      }
    }
  }

  /**
   * Node `node` receives some solutions `ts` from a child node
   * at direction `dir`. If `node` is `None`, we can't go higher up
   * and we just output solutions here. Otherwise we propagate a level
   * higher
   */
  def backPropagate[T](
      receiver: Option[Node[T]],
      ts: Stream[T],
      queue: Queue[Node[T]],
      pos: Int): Stream[T] = receiver match {

    case Some(node) => node match {
      case andNode @ AndNode(parent, pos, cs, f) => {
        /**
         * based on where the notification came from
         * compute possible resulting nodes
         * Also, as a side effect, left or right solutions is updated
         */
        val newSolutions = (dir match {
          case Left =>
            andNode.leftSolutions ++= ts
            for (l <- ts; r <- andNode.rightSolutions) yield f(l, r)

          case Right =>
            andNode.rightSolutions ++= ts
            for (l <- andNode.leftSolutions; r <- ts) yield f(l, r)

          case _ => sys.error("AndNode can't have a Down Child")
        }).toStream

        backPropagate(parent, newSolutions, queue, d)
      }

      /**
       * if I have no parent, I can output the nodes forwarded to me
       * ATTENTION! cannot use `++` because it is inherited from eager collections
       * use `#:::` for preserving the laziness aspect
       */
      case OrNode(parent, pos, _, _) => backPropagate(parent, ts, queue, pos)
      case t: Terminal[_] =>
        sys.error("a terminal cannot be a receiver in backprop")
    }

    case None => ts #::: loop(queue)
  }

  def bfTraverse[T](rootNode: Node[T]): Stream[T] = loop(Queue(rootNode))

  def cartesianProduct[T](ls: List[List[T]]): List[List[T]] = ls match {
    case Nil => Nil
    case xs :: Nil => for (x <- xs) yield List(x)
    case xs :: xss => for (lst <- cartesianProduct(xss); x <- xs) yield (x :: lst)
  }
}

/**
 * def loop(ls: Queue[Node[T]]): Stream[T] = {
  if (ls.isEmpty) Stream.empty
  else {
    val (hd, tl) = ls.dequeue
    hd match {
      case t: Terminal => backPropagate(t.parent, t.values, tl, t.pos)
      case _           =>
        val children = hd.children.copyWith(...)
        loop(tl ++ children)
    }
  }
}

def backPropagate[T](
    node: Node[_],
    values: List[T],
    rest: Queue[Node[_]],
    pos: Int): Stream[_] = node match {

  case a: AndNode =>
    val newSolutions = a.storeAndGetNewSols(values, pos)
  case _ =>

  node.parent match {
    case None => newSolutions #::: loop(queue)
    case Some(p) => backPropagate(p, newSolutions, rest, p.pos)
  }

}
 */
