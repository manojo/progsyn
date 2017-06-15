package traversal

import graph._
import scala.collection.immutable.Queue

trait Traversal {
  def bfTraverse[T](rootNode: Node[T]): Stream[T] = {
    import Node._

    /**
     * The inner loop doing a breadth-first traversal over a node
     * structure
     * Since we are generating a grammar, we will always add new children
     * for every node we see, setting their parent the node we just have observed
     * This will ensure that the right parent is preserved at all stages
     */
    def loop(ls: Queue[Node[T]]): Stream[T] = {
      if (ls.isEmpty) Stream.empty
      else {
        val (hd, tl) = ls.dequeue
        hd match {
          case AndNode(p, d, l, r, _) =>
            val newL = copy(l, p = Some(hd), d = Left)
            val newR = copy(r, p = Some(hd), d = Right)
            loop(tl :+ newL :+ newR)
          
          case OrNode(p, d, l, r)  =>
            val newL = copy(l, p = Some(hd), d = Left)
            val newR = copy(r, p = Some(hd), d = Right)
            loop(tl :+ newL :+ newR)

          case Terminal(v, p, d)  => p match {
            case Some(parent) => backPropagate(parent, v #:: Stream.empty, tl, d)
            case None         => v #:: loop(tl)
          }
        }
      }
    }

    def backPropagate(
        node: Node[T],
        ts: Stream[T],
        queue: Queue[Node[T]],
        dir: Direction): Stream[T] = node match {
      
      case andNode @ AndNode(parent, d, _, _, f) => {
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

        parent match {
          /**
           * Since no parent node, all I need to do is to cross product
           * these solutions with those on the other side, and ship
           * the results out
           * ATTENTION! cannot use `++` because it is inherited from eager collections
           * use `#:::` for preserving the laziness aspect
           */
          case None => newSolutions #::: loop(queue)
          case Some(p) => backPropagate(p, newSolutions, queue, d)
        }
      }
        
      /**
       * if I have no parent, I can output the nodes forwarded to me
       * ATTENTION! cannot use `++` because it is inherited from eager collections
       * use `#:::` for preserving the laziness aspect
       */
      case OrNode(parent, d, _, _) => parent match {
        case None => ts #::: loop(queue)
        case Some(p) => backPropagate(p, ts, queue, d)
      }
      
      case Terminal(v, p, d) => 
        sys.error("can't back propagate to a Terminal")
    }
    
    loop(Queue(rootNode))
  }
}