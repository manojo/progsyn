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

  def solve(spec: Spec[String]): Stream[Exp] = {
    genExp(spec).filter {
      substr => spec.exampleSpec.forall { case (in, out) => eval(substr, in) == Some(out) }
    }
  }

  def genExp(spec: Spec[String]): Stream[Exp] =
    genSubstring(spec)

  def genSubstring(spec: Spec[String]): Stream[Exp]

  /**===================== SPECIFICATIONS =================================*/
  /**
   * Given a specification for Substring, can I get a specification for pos?
   * i.e. I need to find out which positions are of interest.
   */
  def specForPos(spec: Spec[String]): Spec[Set[(Int, Int)]]

  /**
   * Given an idx, where could the parameter of AbsPos come from?
   */
  def specForAbsPos(spec: Spec[Set[Int]]): Spec[Set[Int]]

  /**
   * given an idx, what regex-es could have matched, and where?
   */
  def specForRegexPos(spec: Spec[Set[Int]]): Spec[Set[(Regex, Regex, Int)]]

  /**====================== CONTEXT =======================================*/
  /**
   * A class representing a Specification
   * A specification is made of examples, and a formal constraint
   * The input is fixed to be string here, for now.
   */
  case class Spec[T](exampleSpec: List[(String, T)], constraint: Bool)
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
  def genSubstring(spec: Spec[String]): Stream[Exp] = {
    val posSpec: Spec[Set[(Int, Int)]] = specForPos(spec)
    genPos(posSpec).map { case (pos1, pos2) => Substring(StrSym, pos1, pos2) }
  }

  def genPos(spec: Spec[Set[(Int, Int)]]): Stream[(Pos, Pos)] = {
    val leftInts = spec.exampleSpec.map { case (str, ls) => (str, ls.map(_._1)) }
    val rightInts = spec.exampleSpec.map { case (str, ls) => (str, ls.map(_._2)) }

    val leftPossiblePoses =
      genAbsPos(Spec(leftInts, spec.constraint)) #::: genRegPos(Spec(leftInts, spec.constraint))
    val rightPossiblePoses =
      genAbsPos(Spec(rightInts, spec.constraint)) #::: genRegPos(Spec(rightInts, spec.constraint))

    for (lPos <- leftPossiblePoses; rPos <- rightPossiblePoses) yield (lPos, rPos)
  }

  /**
   * Among the input strings, reverse engineer the position, and keep those
   * which appear in all examples
   */
  def genAbsPos(spec: Spec[Set[Int]]): Stream[Pos] = {
    val possibleInts: Spec[Set[Int]] = specForAbsPos(spec)
    val onlyIdxes = possibleInts.exampleSpec.map(_._2)
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
  def genRegPos(spec: Spec[Set[Int]]): Stream[Pos] = {
    val possibleRegs: Spec[Set[(Regex, Regex, Int)]] = specForRegexPos(spec)
    val onlyTriples = possibleRegs.exampleSpec map (_._2)
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
   *
   * The set of constraints we have don't apply here, so we return it as is
   */
  def specForPos(spec: Spec[String]): Spec[Set[(Int, Int)]] = {
    val exSpec = for ((input, output) <- spec.exampleSpec) yield {
      (input, output.r.findAllMatchIn(input).map(x => (x.start, x.end)).toSet)
    }
    Spec(exSpec, spec.constraint)
  }

  /**
   * Given an idx, where could the parameter of AbsPos come from?
   * It could either be the idx itself, or, since we accept negative indices
   * (for referring to positions from the back of the string), the relevant
   * opposite
   */
  def specForAbsPos(spec: Spec[Set[Int]]): Spec[Set[Int]] = {
    val exSpec = for ((inputstr, idxes) <- spec.exampleSpec) yield {
      (inputstr, idxes.flatMap(idx => Set(idx, inputstr.length - idx)))
    }
    Spec(exSpec, spec.constraint)
  }

  /**
   * given an idx, what regex-es could have matched, and where?
   */
  def specForRegexPos(spec: Spec[Set[Int]]): Spec[Set[(Regex, Regex, Int)]] = {

    val allowedRegexes: List[Regex] = List(
      "".r, //empty
      "\\d".r, //digits
      "\\b\\d+\\b".r,
      "[a-zA-Z]".r, //letters
      "\\b[a-zA-Z]+\\b".r,
      "\\s|^|$".r, //spaces
      "\\s+".r
    )

    val inputsOnly = spec.exampleSpec.map(_._1)

    // filter all regexes from the list which match at least once
    // in all examples
    val relevantRegexes = allowedRegexes.filter { regex =>
      inputsOnly.forall { in => !regex.findFirstIn(in).isEmpty }
    }.toSet

    val exSpec = for ((inputstr, idxes) <- spec.exampleSpec) yield {
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
    Spec(exSpec, spec.constraint)
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
      ("abc 124 def 247 ghi 77854", "def"),
      ("124 asdfasdf 123a abc 232", "abc")
    )

    val constraint = Matches(StrSym, "\\d+".r)

    println(solve(Spec(examples, constraint)).take(5).toList)
  }
}
