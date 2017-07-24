package progsyn

import scala.util.matching.Regex

trait StringLanguage {

  case object StrSym

  sealed trait Exp
  case class Substring(v: StrSym.type, l: Pos, r: Pos) extends Exp

  sealed trait Pos
  case class AbsPos(v: StrSym.type, i: Int) extends Pos
  //case class RegexPos(v: String, r: Regex, p: Int) extends Pos

  def eval(e: Exp, s: String): String = e match {
    case Substring(_, l, r) => s.substring(eval(l), eval(r))
  }

  def eval(p: Pos): Int = p match {
    case AbsPos(_, i) => i
    //case RegexPos(v, )
  }

  def solve(examples: List[(String, String)]): Stream[Exp] =
    genExp(examples)

  def genExp(examples: List[(String, String)]): Stream[Exp] =
    genSubstring(examples)

  def genSubstring(examples: List[(String, String)]): Stream[Exp]
}

/**
 * Solution 0: naive substring generation. have a stream of pairs of integers, and
 * generate substrings for all of these
 * The problem with naive solution: enumeration strategy for pairs can yield invalid
 * pairs very early (invalid, i.e. longer than input string)
 * Domain knowledge is required very early on.
 */
trait Solution0 extends StringLanguage {
  def genSubstring(examples: List[(String, String)]): Stream[Exp] = {
    genPair
      .filter { case (l, r) => l < r }
      .map { case (st, end) => Substring(StrSym, AbsPos(StrSym, st), AbsPos(StrSym, end)) }
      .filter(substr => examples.forall { case (in, out) => eval(substr, in) == out })
  }

  def genPair: Stream[(Int, Int)] = {
    def from(i: Int): Stream[Int] = i #:: from(i + 1)

    def pairsOfSize(n: Int): Stream[(Int, Int)] =
      (for(i <- 0 to n) yield (i, n - i)).toStream

    from(1) flatMap pairsOfSize
  }
  // genSubstring(List(("hehe", "he"))) will yield only one solution, (0, 2)
  // and will not be able to get to (2, 4)
}

/**
 * Solution 1.
 * A bit less naive string generation: we bake in the fact that we only want
 * positive pairs where the second element is larger (saves generation time)
 *
 * we only enumerate pairs such that max length is not crossed. This is done by
 * looking at the shortest string in the inputs.
 *
 * Problem: this does not cover substrings where we just take index to end of string
 */
trait Solution1 extends StringLanguage {
  def genSubstring(examples: List[(String, String)]): Stream[Exp] = {
    val smallestLen = examples.minBy(pair => pair._1.length)._1.length
    println(smallestLen)

    genPair(smallestLen)
      .map { case (st, end) => Substring(StrSym, AbsPos(StrSym, st), AbsPos(StrSym, end)) }
      .filter(substr => examples.forall { case (in, out) => eval(substr, in) == out })
  }

  def genPair(maxSize: Int): Stream[(Int, Int)] = {
    def from(i: Int): Stream[Int] = {
      if (i > maxSize) Stream.empty
      else i #:: from(i + 1)
    }

    def pairsWith(n: Int): Stream[(Int, Int)] =
      (for (i <- 0 to n) yield (i, n)).toStream

    from(1) flatMap pairsWith
  }

  // genSubstring(List(("hehe", "he"))) will yield both solutions, (0, 2), (2, 4)
}


object StringExtract extends Solution1 {

  def main(args: Array[String]): Unit = {
    println("Hi!")
    println(genSubstring(List(("hehe", "he"))).take(10).toList)
  }
}
