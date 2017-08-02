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


  /**===================== SEMANTICS ===========================================*/
  def eval(e: Exp, s: String): Option[String] = e match {
    case Substring(_, l, r) => for {
      i <- eval(l, s)
      j <- eval(r, s)
    } yield funkysub(s, i, j)
  }

  /**
   * returns the empty string if positions don't match
   * negative indices are used to indicate going backwards
   * ex: funkysub("hello", -2, -1) should yield "o"
   */
  def funkysub(s: String, start: Int, end: Int): String = {
    def absidx(k: Int, len: Int): Int = if (k >= 0) k else (len + k + 1)
    s.slice(absidx(start, s.length), absidx(end, s.length))
  }

  def eval(p: Pos, s: String): Option[Int] = p match {
    case AbsPos(_, i) => Some(i)
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
}
