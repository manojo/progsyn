package progsyn

import traversal._
import graph._
import grammar._

/**
 * Defining the add language
 */
trait AddLanguage extends Grammars {

  type Input
  def input: Input

  type Output

  abstract class Exp
  case class Add(l: Exp, r: Exp) extends Exp
  case class Lookup(i: Int) extends Exp

  def s: Grammar[Exp] = (t ~ s).map { case (l, r) => Add(l, r) } | t
  def t: Grammar[Exp] = natGen map Lookup

  def eval(e: Exp): Output

}

object ProgSyn extends Traversal with AddLanguage {

  type Input = Array[Int]
  def input = Array(1, 2, 3)

  type Output = Int

  def eval(e: Exp): Int = e match {
    case Add(l, r) => eval(l) + eval(r)
    case Lookup(i) => input(i)
  }

  def main(args: Array[String]): Unit = {
    println("Hi!")

    /**
     * s := (t ~ s) map f | t
     * t := Lookup
     */
    val t: Node[Exp] = Terminal(Lookup(1) #:: Stream.empty, None, 1)
    val rootNode: Node[Exp] = OrNode(None, -1, Array(null, t))
    val andNode: Node[Exp] = AndNode(
      None, 1, Array(t, rootNode),
      { case l :: r :: _ => Add(l, r) })
    rootNode match { case o: OrNode[_] => o.children(0) = andNode }

    println(bfTraverse(rootNode).take(4).toList)
    val lst = List(List(1, 2), List(3, 4), List(5, 6))
  }
}
