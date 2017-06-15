package progsyn

import traversal._
import graph._

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

object ProgSyn extends Traversal {
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

