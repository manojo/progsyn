package progsyn

import scala.util.matching.Regex

/**
 * A redesign of `StringLanguage` from earlier
 * The goal is to make the `witness` functions more visible, rather
 * than baking them into the implementation of generators
 */
trait StringLang2 {

  /** A string symbol */
  case object StrSym

  /** A substring expression */
  sealed trait Exp
  case class Substring(v: StrSym.type, l: Pos, r: Pos) extends Exp

  /** A position in a substring */
  sealed trait Pos
  case class AbsPos(v: StrSym.type, i: Int) extends Pos
  /**
   * A position represented by the ith match
   * of lreg and rreg. The position, in int, corresponds to the position
   * where lreg ends and rreg starts (with nothing in the middle)
   */
  case class RegexPos(v: StrSym.type, lreg: Regex, rreg: Regex, num: Int) extends Pos


  /**===================== SEMANTICS =========================================*/
  def eval(e: Exp, s: String): Option[String] = e match {
    case Substring(_, l, r) => for {
      i <- eval(l, s)
      j <- eval(r, s)
    } yield s.slice(i, j)
  }

  def eval(p: Pos, s: String): Option[Int] = p match {
    case AbsPos(_, i) => Some(if (i >= 0) i else (s.length + i + 1))
    case RegexPos(v, lreg, rreg, num) =>

      /**
       * all matches of lreg, we assume they're in ascending order
       */
      val lregEndings = lreg.findAllMatchIn(s).map(_.end)
      val rregBeginnings = rreg.findAllMatchIn(s).map(_.start).toSet

      val relevantPoses =
        lregEndings.filter(pos => rregBeginnings.contains(pos)).toArray

      val len = relevantPoses.length

      if (len == 0) None
      else if (num >= 0 && num < len) Some(relevantPoses(num))
      else if (num < 0 && -num <= len) Some(relevantPoses(len + num))
      else None
  }

  def solve(examples: List[(String, String)]): Stream[Exp] =
    genExp(examples)

  def genExp(examples: List[(String, String)]): Stream[Exp] =
    genSubstring(examples)

  def genSubstring(examples: List[(String, String)]): Stream[Exp]

  /**===================== SPECIFICATIONS =================================*/
  /**
   * Given a specification for Substring, can I get a specification for pos?
   * i.e. I need to find out which positions are of interest.
   */
  def specForPos(examples: List[(String, String)]): List[(String, Set[(Int, Int)])]

  /**
   * Given an idx, where could the parameter of AbsPos come from?
   */
  def specForAbsPos(spec: List[(String, Set[Int])]): List[(String, Set[Int])]

  /**
   * given an idx, what regex-es could have matched, and where?
   */
  def specForRegexPos(spec: List[(String, Set[Int])]): List[(String, Set[(Regex, Regex, Int)])]
}

/**
 * This trait implements the generation of substrings (interface provided above)
 * These methods are fairly straightforward and generic to implement, and ideally
 * shouldn't require a developer (they should rather be taken care of by the
 * framework)
 */
trait Generators extends StringLang2 {

  /**
   * Based on the above specs we can create a generator for substrings
   */
  def genSubstring(examples: List[(String, String)]): Stream[Exp] = {
    val posSpec: List[(String, Set[(Int, Int)])] = specForPos(examples)
    genPos(posSpec).map { case (pos1, pos2) => Substring(StrSym, pos1, pos2) }
  }

  def genPos(posSpec: List[(String, Set[(Int, Int)])]): Stream[(Pos, Pos)] = {
    val leftInts = posSpec.map { case (str, ls) => (str, ls.map(_._1)) }
    val rightInts = posSpec.map { case (str, ls) => (str, ls.map(_._2)) }

    val leftPossiblePoses = genAbsPos(leftInts) #::: genRegPos(leftInts)
    val rightPossiblePoses = genAbsPos(rightInts) #::: genRegPos(rightInts)

    for (lPos <- leftPossiblePoses; rPos <- rightPossiblePoses) yield (lPos, rPos)
  }

  /**
   * Among the input strings, reverse engineer the position, and keep those
   * which appear in all examples
   */
  def genAbsPos(posSpec: List[(String, Set[Int])]): Stream[Pos] = {
    val possibleInts: List[(String, Set[Int])] = specForAbsPos(posSpec)
    val onlyIdxes = possibleInts.map(_._2)
    val relevantIdxes: Set[Int] = onlyIdxes match {
      case Nil => Set.empty
      case x :: xs => xs.foldLeft(x){ case (acc, ls) =>
        acc intersect ls
      }
    }
    relevantIdxes.map(x => AbsPos(StrSym, x)).toStream
  }

  /**
   * Among the regex and index triples, pick those that appear in all
   * examples
   */
  def genRegPos(posSpec: List[(String, Set[Int])]): Stream[Pos] = {
    val possibleRegs: List[(String, Set[(Regex, Regex, Int)])] = specForRegexPos(posSpec)
    val onlyTriples = possibleRegs map (_._2)
    val relevantTriples: Set[(Regex, Regex, Int)] = onlyTriples match {
      case Nil => Set.empty
      case x :: xs => xs.foldLeft(x){ case (acc, ls) =>
        acc intersect ls
      }
    }
    relevantTriples.map { case (r1, r2, idx) => RegexPos(StrSym, r1, r2, idx) }.toStream
  }

}
