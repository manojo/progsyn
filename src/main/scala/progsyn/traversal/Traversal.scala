package progsyn.traversal

import scala.reflect.ClassTag

import progsyn.graph._
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
  private def loop[T: ClassTag](ls: Queue[Node[T]]): Stream[T] = {
    if (ls.isEmpty) Stream.empty
    else {
      val (hd, tl) = ls.dequeue
      hd match {
        case Terminal(vs, p, pos)  =>
          backPropagate(p, vs, tl, pos)

        case _ =>
          val newChildren = hd.children.zipWithIndex map { case (child, idx) =>
            copy(child, p = Some(hd), pos = idx)
          }
          loop(tl ++ newChildren)
      }
    }
  }

  /**
   * Node `node` receives some solutions `ts` from a child node
   * at direction `dir`. If `node` is `None`, we can't go higher up
   * and we just output solutions here. Otherwise we propagate a level
   * higher
   */
  def backPropagate[T: ClassTag](
      receiver: Option[Node[T]],
      ts: Stream[T],
      queue: Queue[Node[T]],
      pos: Int): Stream[T] = receiver match {

    case Some(node) => node match {
      case _: Terminal[_] =>
        sys.error("a terminal cannot be a receiver in backprop")

      case andNode @ AndNode(parent, p, children, f) => {
        /**
         * based on where the notification came from
         * compute possible resulting nodes
         * Also, as a side effect, the solutions at `pos` are updated
         */
        val beforePos: List[List[T]] =
          (for (i <- 0 until pos) yield andNode.solutions(i).toList).toList

        val afterPos: List[List[T]] = (
          for (i <- (pos + 1) until andNode.solutions.length) yield
            andNode.solutions(i).toList).toList

        val candidates = beforePos ++ ((ts.toList) :: afterPos)
        val newSolutions = cartesianProduct(candidates).map(f).toStream

        andNode.solutions(pos) ++= ts

        backPropagate(parent, newSolutions, queue, p)
      }

      /**
       * if I have no parent, I can output the nodes forwarded to me
       * ATTENTION! cannot use `++` because it is inherited from eager collections
       * use `#:::` for preserving the laziness aspect
       */
      case OrNode(parent, p, _) => backPropagate(parent, ts, queue, p)
    }

    case None => ts #::: loop(queue)
  }

  def bfTraverse[T: ClassTag](rootNode: Node[T]): Stream[T] = loop(Queue(rootNode))

  def cartesianProduct[T](ls: List[List[T]]): List[List[T]] = ls match {
    case Nil => Nil
    case xs :: Nil => for (x <- xs) yield List(x)
    case xs :: xss => for (lst <- cartesianProduct(xss); x <- xs) yield (x :: lst)
  }
}
