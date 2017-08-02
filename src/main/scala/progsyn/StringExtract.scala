package progsyn

import scala.util.matching.Regex

trait StringLanguage {

  case object StrSym

  sealed trait Exp
  case class Substring(v: StrSym.type, l: Pos, r: Pos) extends Exp

  sealed trait Pos
  case class AbsPos(v: StrSym.type, i: Int) extends Pos

  def eval(e: Exp, s: String): Option[String] = e match {
    case Substring(_, l, r) => for {
      i <- eval(l, s)
      j <- eval(r, s)
    } yield s.substring(i, j)
  }

  def eval(p: Pos, s: String): Option[Int] = p match {
    case AbsPos(_, i) => Some(i)
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
      .filter(substr => examples.forall { case (in, out) => eval(substr, in) == Some(out) })
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
      .filter(substr => examples.forall { case (in, out) => eval(substr, in) == Some(out) })
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

/**
 * Solution 3: growing the space of substrings. We want to be able to refer to
 * substrings such as "from position (i) till the end"
 * For this we also have to redefine eval, because the generic `substring` from
 * java only deals with increasing ranges.
 */
trait Solution3 extends StringLanguage {

  override def eval(e: Exp, s: String): Option[String] = e match {
    case Substring(_, l, r) => for {
      i <- eval(l, s)
      j <- eval(r, s)
    } yield funkysub(s, i, j)
  }

  //returns the empty string if positions don't match
  //negative indices are used to indicate going backwards
  // ex: funkysub("hello", -2, -1) should yield "o"
  def funkysub(s: String, start: Int, end: Int): String = {

    def absidx(k: Int, len: Int): Int = if (k >= 0) k else (len + k + 1)
    s.slice(absidx(start, s.length), absidx(end, s.length))
  }

  def genSubstring(examples: List[(String, String)]): Stream[Exp] = {
    val smallestLen = examples.minBy(pair => pair._1.length)._1.length

    genPair(smallestLen)
      .map { case (st, end) => Substring(StrSym, AbsPos(StrSym, st), AbsPos(StrSym, end)) }
      .filter(substr => examples.forall { case (in, out) => eval(substr, in) == Some(out) })
  }

  def genPair(maxSize: Int): Stream[(Int, Int)] = {
    def from(i: Int): Stream[Int] = {
      if (i > maxSize) Stream.empty
      else i #:: from(i + 1)
    }

    def pairsWith(n: Int): Stream[(Int, Int)] =
      (for (i <- 0 to n) yield (i, n)).toStream

    def pairsWithNeg(n: Int): Stream[(Int, Int)] =
      (for (i <- (n + 1) to (maxSize + 1)) yield (-i, -n)).toStream

    def pairsPosandNeg(n: Int): Stream[(Int, Int)] =
      (for (i <- 0 to maxSize + 1) yield (i, -n)).toStream

    from(1).flatMap { i => pairsWith(i) #::: pairsWithNeg(i) #::: pairsPosandNeg(i) }
  }

  // genSubstring(List(("hehe", "he"))) will yield 6 solutions, (-3, -1), (0, 2), (-5, -3), (2, 4), (0, -3), (2, -1)
}

/**
 * Introducing a new way to get positions, using regex positions, to match
 * beginning and end positions
 */
trait Solution4 extends Solution3 {

  /**
   * A position represented by the ith match
   * of lreg and rreg. The position, in int, corresponds to the position
   * where lreg ends and rreg starts (with nothing in the middle)
   */
  case class RegexPos(v: StrSym.type, lreg: Regex, rreg: Regex, num: Int) extends Pos

  /**
   * Store of regexes
   */
  val allowedRegexes: List[Regex] = List(
    "".r, //empty
    "\\d".r, //digits
    "\\d+".r,
    "[a-zA-Z]".r, //letters
    "[a-zA-Z]+".r,
    "\\s".r, //spaces
    "\\s+".r
  )


  override def eval(p: Pos, s: String): Option[Int] = p match {
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

    case _ => super.eval(p, s)
  }

  /**
   * generating a substring means either generating abspositions
   * and regexpositions
   * Generating regex positions amounts to doing a cross product on available
   * regexes and their positions
   */
  override def genSubstring(examples: List[(String, String)]): Stream[Exp] = {

    val smallestLen = examples.minBy(pair => pair._1.length)._1.length

    val regPoses = genRegexPos(examples)

    val possibleSubstrs: Stream[Exp] = {
      (for (r1 <- regPoses; r2 <- regPoses) yield Substring(StrSym, r1, r2))
      .filter(substr => examples.forall { case (in, out) => eval(substr, in) == Some(out) })
    }

    super.genSubstring(examples) #::: possibleSubstrs
  }

  def genRegexPos(examples: List[(String, String)]): Stream[RegexPos] = {

    // filter all regexes from the list which match at least once
    // in all examples
    val relevantRegexes = allowedRegexes.filter { regex =>
      examples.map(_._1).forall { in =>
        !regex.findFirstIn(in).isEmpty
      }
    }.toSet

    // find the max possible number of regex matches
    // since the empty string (epsilon) will always be matched
    // this is the same as finding the shortest example input string
    val smallestLen = examples.minBy(pair => pair._1.length)._1.length

    val regexPoses = (for {
      r <- relevantRegexes
      r2 <- relevantRegexes - r
      idx <- (0 until smallestLen) ++ (1 to smallestLen).map(x => -x)
    } yield RegexPos(StrSym, r, r2, idx)).toStream

    regexPoses
  }
}

object StringExtract extends Solution4 {

  def main(args: Array[String]): Unit = {
    println("Hi!")

    val examples = List(
      ("abc def ghi", "def"),
      ("asdfasdf 123a asdfasdf", "123a")
    )

    println(genSubstring(examples).take(10).toList)

  }
}
