package progsyn
/**
trait Grammars {
  type Input = String

abstract class Grammar[+T] extends (() => Stream[T]) { self =>

    def ~[U](that: => Grammar[U]): Grammar[(T, U)] =
      And(this, mkGrammar{() => that()})

    def | [U >: T](that: => Grammar[U]): Grammar[U] =
      Or(this, mkGrammar{() => that()})

    def map[U](f: T => U): Grammar[U] =
      Map(this, f)
  }

  def mkGrammar[T](f: () => Stream[T]) = new Grammar[T] {
    def apply(): Stream[T] = f()
  }

  case class And[T, U](g1: Grammar[T], g2: Grammar[U]) extends Grammar[(T, U)] {
    def apply(): Stream[(T, U)] = for (l <- g1(); r <- g2()) yield (l, r)
  }

  case class Or[T, U >: T](g1: Grammar[T], g2: Grammar[U]) extends Grammar[U] {
    def apply(): Stream[U] = g1() ++ g2()
  }

  case class Map[T, U](g: Grammar[T], f: T => U) extends Grammar[U] {
    def apply(): Stream[U] = g().map(f)
  }

  case class Just[T](t: T) extends Grammar[T] {
    def apply() = t #:: Stream.empty
  }

  def unit[T](t: T): Grammar[T] = Just(t)

  abstract class Exp
  case class Add(l: Exp, r: Exp) extends Exp
  case object Lookup extends Exp

  def eval(e: Exp): Int = e match {
    case Lookup => 0
    case Add(l, r) => eval(l) + eval(r)
  }

}
*/
import scala.collection.immutable.Queue

object ProgSyn {

  abstract class Direction
  case object Left extends Direction
  case object Right extends Direction
  case object Down extends Direction

  abstract class Node[T] {
    var parent: Option[Node[T]]
    /** Indicates if this is the left, right or down child of the parent */
    val dir: Direction

    def notify(
      ts: Stream[T],
      queue: Queue[Node[T]],
      k: Queue[Node[T]] => Stream[T],
      dir: Direction): Stream[T]
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

    def notify(
        ts: Stream[T],
        queue: Queue[Node[T]],
        k: Queue[Node[T]] => Stream[T],
        dir: Direction): Stream[T] = {

      /**
       * based on where the notification came from
       * compute possible resulting nodes
       * Also, as a side effect, left or right solutions is updated
       */
      val newSolutions = (dir match {
        case Left => leftSolutions ++= ts; for (l <- ts; r <- rightSolutions) yield builder(l, r)
        case Right => rightSolutions ++= ts; for (l <- leftSolutions; r <- ts) yield builder(l, r)
        case _ => ??? //An AndNode should not have a Down child
      }).toStream

      parent match {

        /**
         * Since no parent node, all I need to do is to cross product
         * these solutions with those on the other side, and ship
         * the results out
         * ATTENTION! cannot use `++` because it is inherited from eager collections
         * use `#:::` for preserving the laziness aspect
         */
        case None => newSolutions #::: k(queue)
        case Some(p) => p.notify(newSolutions, queue, k, this.dir)
      }

    }
  }

  case class OrNode[T](
      var parent: Option[Node[T]],
      dir: Direction,
      var leftChild: Node[T],
      rightChild: Node[T]) extends Node[T] {

    def notify(
      ts: Stream[T],
      queue: Queue[Node[T]],
      k: Queue[Node[T]] => Stream[T],
      dir: Direction): Stream[T] = parent match {

      /**
       * if I have no parent, I can output the nodes forwarded to me
       * ATTENTION! cannot use `++` because it is inherited from eager collections
       * use `#:::` for preserving the laziness aspect
       */
      case None => ts #::: k(queue)
      case Some(p) => p.notify(ts, queue, k, this.dir)
    }
  }

  case class Terminal[T](
      value: T,
      var parent: Option[Node[T]],
      dir: Direction) extends Node[T] {

    /**
     * Notify should never be triggered
     * for a terminal
     */
    def notify(
      ts: Stream[T],
      queue: Queue[Node[T]],
      k: Queue[Node[T]] => Stream[T],
      dir: Direction): Stream[T] = ???

  }

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
            assert(!(newL eq l))
            assert(!(newR eq r))
            loop(tl :+ newL :+ newR)

          case OrNode(p, d, l, r)  =>
            val newL = copy(l, p = Some(hd), d = Left)
            val newR = copy(r, p = Some(hd), d = Right)
            assert(!(newL eq l))
            assert(!(newR eq r))
            loop(tl :+ newL :+ newR)

          case Terminal(v, p, d)  => p match {
            case Some(parent) => parent.notify(v #:: Stream.empty, tl, loop _, d)
            case None         => v #:: loop(tl)
          }
        }
      }
    }

    loop(Queue(rootNode))
  }

  def main(args: Array[String]): Unit = {
    println("Hi!")

    abstract class Exp
    case class Add(l: Exp, r: Exp) extends Exp
    case object Lookup extends Exp

    /**
     * s := (t ~ s) map f | t
     * t := Lookup
     */

    val t: Node[Exp] = Terminal(Lookup, None, Right)
    val rootNode: Node[Exp] = OrNode(None, Down, null, t)
    val andNode: Node[Exp] = AndNode(None, Left, t, rootNode, (l, r) => Add(l, r))

    rootNode match { case o: OrNode[_] => o.leftChild = andNode }


    println(bfTraverse(rootNode).take(4).toList)
    //println("prioritytraverse now")
    //priorityTraverse(nodes).foreach(x => println(x))
    //println(from(3))
  }
}

