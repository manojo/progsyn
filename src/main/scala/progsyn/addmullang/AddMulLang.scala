package progsyn.addmullang

trait AddMulLang { self: ConstraintLang =>

  /** symbols representing variables of certain types */
  case object ArrSym

  /** Arithmetic expressions */
  sealed trait Exp
  case class Add(v: ArrSym.type, l: Exp, r: Exp) extends Exp
  case class Lookup(v: ArrSym.type, p: Pos) extends Exp

  /** A position in an array */
  sealed trait Pos
  case class AbsPos(v: ArrSym.type, i: Int) extends Pos

  /**===================== SEMANTICS =========================================*/
  def eval(e: Exp, in: Array[Int]): Option[Int] = e match {
    case Add(_, l, r) => for {
      i <- eval(l, in)
      j <- eval(r, in)
    } yield i + j

    case Lookup(_, p) => for (i <- eval(p, in)) yield in(i)
  }

  def eval(p: Pos, in: Array[Int]): Option[Int] = p match {
    case AbsPos(_, i) => Some(if (i >= 0) i else (in.length + i))
  }

  def solve(spec: Spec[Int]): Stream[Exp] = genExp(spec).filter {
    exp => spec.exampleSpec.forall { case (in, out) => eval(exp, in) == Some(out) }
  }

  def genExp(spec: Spec[Int]): Stream[Exp]

  /**===================== SPECIFICATIONS =================================*/
  /**
   * Given a specification for Lookup, I need a spec for positions
   * i.e. I need to find out which positions are of interest.
   */
  def specForPos(spec: Spec[Int]): Spec[Set[Int]]
  def specForAdd(spec: Spec[Int]): Spec[Set[(Int, Int)]]

  /**====================== CONTEXT =======================================*/
  /**
   * A class representing a Specification
   * A specification is made of examples, and a formal constraint
   * The input is fixed to be string here, for now.
   */
  case class Spec[T](exampleSpec: List[(Array[Int], T)], constraint: Bool)
}

trait ConstraintLang { self: AddMulLang with Generators =>
  abstract class Bool {
    def and(that: Bool): Bool = And(this, that)
    def or(that: Bool): Bool = Or(this, that)
  }

  case object True extends Bool
  case object False extends Bool
  case class And(l: Bool, r: Bool) extends Bool
  case class Or(l: Bool, r: Bool) extends Bool
  case object NoConstraint extends Bool

  def eval(b: Bool)(implicit str: String): Boolean = b match {
    case True => true
    case False => false
    case And(l, r) => eval(l) && eval(r)
    case Or(l, r) => eval(l) && eval(r)
    case _ => ??? //<-- TODO: implement full behaviour?
  }
}

trait Generators { self: AddMulLang with ConstraintLang with Specs =>

  def genExp(spec: Spec[Int]): Stream[Exp] =
    genLookup(spec) #::: genAdd(spec)

  def genLookup(spec: Spec[Int]): Stream[Exp] = {
    val posSpec: Spec[Set[Int]] = specForPos(spec)
    genPos(posSpec) map { p => Lookup(ArrSym, p) }
  }

  def genPos(spec: Spec[Set[Int]]): Stream[Pos] = {
    val Spec(examples, constraint) = spec
    val onlyInts: List[Set[Int]] = examples.map(_._2)

    /**
     * among possible positions, take those that
     * appear in all examples
     */
    val possiblePoses: Set[Int] = onlyInts match {
      case Nil => Set.empty
      case x :: xs => xs.foldLeft(x) { case (acc, elems) =>
        acc intersect elems
      }
    }
    possiblePoses.toStream map {i => AbsPos(ArrSym, i) }
  }

  def genAdd(spec: Spec[Int]): Stream[Exp] = Stream.empty

}

trait Specs { self: AddMulLang with ConstraintLang =>

  /**
   * Given examples for addition, give a specification for
   * valid positions
   */
  def specForPos(spec: Spec[Int]): Spec[Set[Int]] = {
    val Spec(examples, constraint) = spec

    val newExamples = for ((in, out) <- examples) yield {
      val occurences = in.zipWithIndex.filter{ case (elem, idx) => elem == out }.toSet
      val idxesOnly: Set[Int] = occurences.flatMap { case (_, idx) => Set(idx,  idx - in.length) }
      (in, idxesOnly)
    }
    Spec(newExamples, constraint)
  }

  def specForAdd(spec: Spec[Int]): Spec[Set[(Int, Int)]] = Spec(Nil, NoConstraint)
}

object PlayGround extends AddMulLang with ConstraintLang with Generators with Specs {
  def main(args: Array[String]): Unit = {
    println("Hi!")

    val examples = List(
      (Array(1,2,3,4,3,2), 2),
      (Array(3,2,3,2,4), 4)
    )

    val constraint = NoConstraint
    println(solve(Spec(examples, constraint)).take(5).toList)
  }
}
