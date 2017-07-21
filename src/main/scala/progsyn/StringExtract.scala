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

  def solve(examples: List[(String, String)]): Stream[Exp] = genExp(examples)

  def genExp(examples: List[(String, String)]): Stream[Exp] = genSubstring(examples)

  def genSubstring(examples: List[(String, String)]): Stream[Exp] = {
    Stream.empty[Exp]// SubString(StrSym, _, _)
  }

  def genStartPos(examples: List[(String, String)]): Stream[Pos] = {

  }

}

object StringExtract extends StringLanguage {

  def main(args: Array[String]): Unit = {
    println("Hi!")
  }
}
