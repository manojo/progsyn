package progsyn.stringlang

import scala.util.matching.Regex

/**
 * A redesign of `StringLanguage` from earlier
 * The goal is to make the `witness` functions more visible, rather
 * than baking them into the implementation of generators
 */
trait StringLang2 { self: ConstraintSpecs =>

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

  def solve(examples: List[(String, String)])(constraint: Bool): Stream[Exp] = {
    genExp(examples)(constraint).filter {
      substr => examples.forall { case (in, out) => eval(substr, in) == Some(out) }
    }
  }

  def genExp(examples: List[(String, String)])(constraint: Bool): Stream[Exp] =
    genSubstring(examples)(constraint)

  def genSubstring(examples: List[(String, String)])(constraint: Bool): Stream[Exp]

  /**===================== SPECIFICATIONS =================================*/
  /**
   * Given a specification for Substring, can I get a specification for pos?
   * i.e. I need to find out which positions are of interest.
   */
  def specForPos(examples: List[(String, String)])(constraint: Bool): List[(String, Set[(Int, Int)])]

  /**
   * Given an idx, where could the parameter of AbsPos come from?
   */
  def specForAbsPos(spec: List[(String, Set[Int])])(constraint: Bool): List[(String, Set[Int])]

  /**
   * given an idx, what regex-es could have matched, and where?
   */
  def specForRegexPos(spec: List[(String, Set[Int])])(constraint: Bool): List[(String, Set[(Regex, Regex, Int)])]
}

/**
 * This trait implements the generation of substrings (interface provided above)
 * These methods are fairly straightforward and generic to implement, and ideally
 * shouldn't require a developer (they should rather be taken care of by the
 * framework)
 */
trait Generators extends StringLang2 { self: ConstraintSpecs =>

  /**
   * Based on the above specs we can create a generator for substrings
   */
  def genSubstring(examples: List[(String, String)])(constraint: Bool): Stream[Exp] = {
    val posSpec: List[(String, Set[(Int, Int)])] = specForPos(examples)(constraint)
    genPos(posSpec)(constraint).map { case (pos1, pos2) => Substring(StrSym, pos1, pos2) }
  }

  def genPos(posSpec: List[(String, Set[(Int, Int)])])(constraint: Bool): Stream[(Pos, Pos)] = {
    val leftInts = posSpec.map { case (str, ls) => (str, ls.map(_._1)) }
    val rightInts = posSpec.map { case (str, ls) => (str, ls.map(_._2)) }

    val leftPossiblePoses = genAbsPos(leftInts)(constraint) #::: genRegPos(leftInts)(constraint)
    val rightPossiblePoses = genAbsPos(rightInts)(constraint) #::: genRegPos(rightInts)(constraint)

    for (lPos <- leftPossiblePoses; rPos <- rightPossiblePoses) yield (lPos, rPos)
  }

  /**
   * Among the input strings, reverse engineer the position, and keep those
   * which appear in all examples
   */
  def genAbsPos(posSpec: List[(String, Set[Int])])(constraint: Bool): Stream[Pos] = {
    val possibleInts: List[(String, Set[Int])] = specForAbsPos(posSpec)(constraint)
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
  def genRegPos(posSpec: List[(String, Set[Int])])(constraint: Bool): Stream[Pos] = {
    val possibleRegs: List[(String, Set[(Regex, Regex, Int)])] = specForRegexPos(posSpec)(constraint)
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

/**
 * This trait implements the specifications declared above
 */
trait Specs extends StringLang2 with Generators { self: ConstraintSpecs =>

  /**
   * Given a specification for Substring, can I get a specification for pos?
   * i.e. I need to find out which positions are of interest.
   */
  def specForPos(examples: List[(String, String)])(constraint: Bool): List[(String, Set[(Int, Int)])] = {
    for ((input, output) <- examples) yield {
      (input, output.r.findAllMatchIn(input).map(x => (x.start, x.end)).toSet)
    }
  }

  /**
   * Given an idx, where could the parameter of AbsPos come from?
   * It could either be the idx itself, or, since we accept negative indices
   * (for referring to positions from the back of the string), the relevant
   * opposite
   */
  def specForAbsPos(spec: List[(String, Set[Int])])(constraint: Bool): List[(String, Set[Int])] = {
    for ((inputstr, idxes) <- spec) yield {
      (inputstr, idxes.flatMap(idx => Set(idx, inputstr.length - idx)))
    }
  }

  /**
   * given an idx, what regex-es could have matched, and where?
   */
  def specForRegexPos(spec: List[(String, Set[Int])])(constraint: Bool): List[(String, Set[(Regex, Regex, Int)])] = {

    val allowedRegexes: List[Regex] = List(
      "".r, //empty
      "\\d".r, //digits
      "\\d+".r,
      "[a-zA-Z]".r, //letters
      "[a-zA-Z]+".r,
      "\\s".r, //spaces
      "\\s+".r
    )

    val inputsOnly = spec.map(_._1)

    // filter all regexes from the list which match at least once
    // in all examples
    val relevantRegexes = allowedRegexes.filter { regex =>
      inputsOnly.forall { in => !regex.findFirstIn(in).isEmpty }
    }.toSet

    for ((inputstr, idxes) <- spec) yield {
      val regexPairs = for {
        r1 <- relevantRegexes
        r2 <- relevantRegexes - r1
      } yield (r1, r2)

      val triplesOfInterest = regexPairs.foldLeft(Set.empty[(Regex, Regex, Int)]) { case (acc, (lreg, rreg)) =>
        val lregEndings = lreg.findAllMatchIn(inputstr).map(_.end)
        val rregBeginnings = rreg.findAllMatchIn(inputstr).map(_.start).toSet

        val relevantPoses =
          lregEndings.filter(pos => rregBeginnings.contains(pos))

        if (relevantPoses.isEmpty) acc
        else {
          val len = relevantPoses.length
          val positions = (0 until len) ++ (-1 to -len by -1)
          acc union (positions.map((lreg, rreg, _)).toSet)
        }
      }

      (inputstr, triplesOfInterest)
    }
  }
}

trait ConstraintSpecs extends StringLang2 with Generators {
  abstract class Bool {
    def and(that: Bool): Bool = And(this, that)
    def or(that: Bool): Bool = Or(this, that)
  }

  case object True extends Bool
  case object False extends Bool
  case class And(l: Bool, r: Bool) extends Bool
  case class Or(l: Bool, r: Bool) extends Bool
  case class Matches(s: StrSym.type, r: Regex) extends Bool

  def eval(b: Bool)(implicit str: String): Boolean = b match {
    case True => true
    case False => false
    case And(l, r) => eval(l) && eval(r)
    case Or(l, r) => eval(l) && eval(r)
    case Matches(_, reg) => reg.findFirstIn(str).isEmpty
  }
}

object PlayGround extends Specs with ConstraintSpecs {

  def main(args: Array[String]) {
    println("oh hai!!")

    val examples = List(
      ("abc 124 def 247 ghi 77854", "247"),
      ("124 asdfasdf 123a asdfasdf 232 ", "232")
    )

    val constraint = Matches(StrSym, "\\d+".r)

    println(solve(examples)(constraint).take(5).toList)
  }
}
