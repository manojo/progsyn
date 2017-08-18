package progsyn.stringlang

import scala.util.matching.Regex

/**
 * A redesign of `StringLanguage` from earlier
 * The goal is to make the `witness` functions more visible, rather
 * than baking them into the implementation of generators
 */
trait StringLang2 { self: ConstraintLang =>

  /** symbols representing variables of certain types */
  case object StrSym
  case object IntSym

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
  def specsForPos(spec: Spec[String]): (Spec[Set[Int]], Spec[Set[Int]])

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

  /** ====================== UTIL ==================================*/
  final val allowedRegexes: List[Regex] = List(
    "".r, //empty
    "\\d".r, //digits
    "\\b\\d+\\b".r,
    "[a-zA-Z]".r, //letters
    "\\b[a-zA-Z]+\\b".r,
    "\\s|^|$".r, //spaces
    "\\s+".r
  )
}

/**
 * This trait implements the generation of substrings (interface provided above)
 * These methods are fairly straightforward and generic to implement, and ideally
 * shouldn't require a developer (they should rather be taken care of by the
 * framework)
 */
trait Generators extends StringLang2 { self: ConstraintLang =>

  /**
   * Based on the above specs we can create a generator for substrings
   */
  def genSubstring(spec: Spec[String]): Stream[Exp] = {
    val (leftSpec, rightSpec) = specsForPos(spec)
    genPos(leftSpec, rightSpec).map { case (pos1, pos2) => Substring(StrSym, pos1, pos2) }
  }

  def genPos(leftSpec: Spec[Set[Int]], rightSpec: Spec[Set[Int]]): Stream[(Pos, Pos)] = {
    val Spec(leftInts, leftConstr) = leftSpec
    val Spec(rightInts, rightConstr) = rightSpec

    val leftPossiblePoses =
      genAbsPos(Spec(leftInts, leftConstr)) #::: genRegPos(Spec(leftInts, leftConstr))
    val rightPossiblePoses =
      genAbsPos(Spec(rightInts, rightConstr)) #::: genRegPos(Spec(rightInts, rightConstr))

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
trait Specs extends StringLang2 with Generators { self: ConstraintLang =>

  /**
   * Given a specification for Substring, can I get a specification for pos?
   * i.e. I need to find out which positions are of interest.
   *
   * The set of constraints we have don't apply here, so we return it as is
   */
  def specsForPos(spec: Spec[String]): (Spec[Set[Int]], Spec[Set[Int]]) = {

    /**
     * We modify the outer constraints to reflect constraints on positions
     */
    val newLeftConstr = spec.constraint match {
      case IsNum(_) => IsNumStart(IntSym)
      case IsWord(_) => IsWordStart(IntSym)
      case _ => NoConstraint //<-- we propagate a no constraint
    }

    val newRightConstr = spec.constraint match {
      case IsNum(_) => IsNumEnd(IntSym)
      case IsWord(_) => IsWordEnd(IntSym)
      case _ => NoConstraint
    }

    val exSpecs: List[(String, Set[(Int, Int)])] = for ((input, output) <- spec.exampleSpec) yield {
      (input, output.r.findAllMatchIn(input).map(x => (x.start, x.end)).toSet)
    }

    val (leftExamples, rightExamples) = (
      exSpecs.map { case (in, tuples) => (in, tuples.map(_._1)) },
      exSpecs.map { case (in, tuples) => (in, tuples.map(_._2)) }
    )

    (Spec(leftExamples, newLeftConstr), Spec(rightExamples, newRightConstr))
  }

  /**
   * Given an idx, where could the parameter of AbsPos come from?
   * It could either be the idx itself, or, since we accept negative indices
   * (for referring to positions from the back of the string), the relevant
   * opposite
   */
  def specForAbsPos(spec: Spec[Set[Int]]): Spec[Set[Int]] = {
    val Spec(exampleSpec, constraint) = spec
    constraint match {
      /** should we send this constraint down further, or just false? */
      case IsNumStart(_) => Spec(Nil, False)
      case IsNumEnd(_) => Spec(Nil, False)
      case IsWordStart(_) => Spec(Nil, False)
      case IsWordEnd(_) =>  Spec(Nil, False)
      case _ =>
        val exSpec = for ((inputstr, idxes) <- exampleSpec) yield {
          (inputstr, idxes.flatMap(idx => Set(idx, idx - inputstr.length - 1)))
        }
        Spec(exSpec, constraint)
    }
  }

  /**
   * given an idx, what regex-es could have matched, and where?
   */
  def specForRegexPos(spec: Spec[Set[Int]]): Spec[Set[(Regex, Regex, Int)]] = {
    val Spec(exampleSpec, constraint) = spec

    val inputsOnly = spec.exampleSpec.map(_._1)

    val regexPairs: Set[(Regex, Regex)] = constraint match {
      case IsNumStart(_) => Set(("\\s|^|$".r, "\\b\\d+\\b".r))
      case IsNumEnd(_) => Set(("\\b\\d+\\b".r, "\\s|^|$".r))

      case IsWordStart(_) => Set(("\\s|^|$".r, "\\b[a-zA-Z]+\\b".r))
      case IsWordEnd(_) => Set(("\\b[a-zA-Z]+\\b".r, "\\s|^|$".r))

      case _ => // <-- should really be case NoConstraint
        val relevantRegexes = allowedRegexes.filter { regex =>
          inputsOnly.forall { in => !regex.findFirstIn(in).isEmpty }
        }.toSet

        for {
           r1 <- relevantRegexes
           r2 <- relevantRegexes - r1
         } yield (r1, r2)
    }

    val exSpec = for ((inputstr, idxes) <- spec.exampleSpec) yield {
      val triplesOfInterest = regexPairs.foldLeft(Set.empty[(Regex, Regex, Int)]) { case (acc, (lreg, rreg)) =>
        val lregEndings = lreg.findAllMatchIn(inputstr).map(_.end)
        val rregBeginnings = rreg.findAllMatchIn(inputstr).map(_.start).toSet

        val relevantPoses = lregEndings.filter(pos => rregBeginnings.contains(pos))

        if (relevantPoses.isEmpty) acc
        else {
          val len = relevantPoses.length
          val positions = (0 until len) ++ (-1 to -len by -1)
          acc union (positions.map((lreg, rreg, _)).toSet)
        }
      }
      (inputstr, triplesOfInterest)
    }
    Spec(exSpec, constraint)
  }
}

trait ConstraintLang { self: StringLang2 with Generators =>
  abstract class Bool {
    def and(that: Bool): Bool = And(this, that)
    def or(that: Bool): Bool = Or(this, that)
  }

  case object True extends Bool
  case object False extends Bool
  case class And(l: Bool, r: Bool) extends Bool
  case class Or(l: Bool, r: Bool) extends Bool
  case class IsNum(s: StrSym.type) extends Bool
  case class IsNumStart(i: IntSym.type) extends Bool
  case class IsNumEnd(i: IntSym.type) extends Bool

  case class IsWord(s: StrSym.type) extends Bool
  case class IsWordStart(i: IntSym.type) extends Bool
  case class IsWordEnd(i: IntSym.type) extends Bool

  case object NoConstraint extends Bool

  def eval(b: Bool)(implicit str: String): Boolean = b match {
    case True => true
    case False => false
    case And(l, r) => eval(l) && eval(r)
    case Or(l, r) => eval(l) && eval(r)
    case IsNum(_) => !("\\b\\d+\\b".r).findFirstIn(str).isEmpty
    case IsWord(_) => !("\\b[a-zA-Z]+\\b".r).findFirstIn(str).isEmpty
    case _ => ??? //<-- TODO: implement full behaviour?
  }
}

object PlayGround extends Specs with ConstraintLang {

  def main(args: Array[String]) {
    println("oh hai!!")

    val examples = List(
      ("bc324a asdf 122d 321 asdf 32234 adsd", "asdf"),
      ("123 jkljkl kll 4234q adf asdfasdf asdf", "kll")
    )

    val constraint = IsWord(StrSym)
    println(solve(Spec(examples, constraint)).take(5).toList)
  }
}
