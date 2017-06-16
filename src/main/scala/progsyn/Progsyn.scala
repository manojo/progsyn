package progsyn

import traversal._
import graph._

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

