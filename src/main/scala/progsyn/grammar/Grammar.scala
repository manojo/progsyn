package progsyn.grammar

trait Grammars {

  abstract class Grammar[+T] extends (() => Stream[T]) { self =>
    def ~[U](that: => Grammar[U]): Grammar[(T, U)] =
      And(this, mkGrammar{() => that()})

    def | [U >: T](that: => Grammar[U]): Grammar[U] =
      Or(this, mkGrammar{() => that()})

    def map[U](f: T => U): Grammar[U] =
      Map(this, f)
  }

  def mkGrammar[T](f: () => Stream[T]) = new Grammar[T] {
    def apply(): Stream[T] = f()
  }

  case class And[T, U](g1: Grammar[T], g2: Grammar[U]) extends Grammar[(T, U)] {
    def apply(): Stream[(T, U)] = for (l <- g1(); r <- g2()) yield (l, r)
  }

  case class Or[T, U >: T](g1: Grammar[T], g2: Grammar[U]) extends Grammar[U] {
    def apply(): Stream[U] = g1() ++ g2()
  }

  case class Map[T, U](g: Grammar[T], f: T => U) extends Grammar[U] {
    def apply(): Stream[U] = g().map(f)
  }

  case class Just[T](t: T) extends Grammar[T] {
    def apply() = t #:: Stream.empty
  }

  def unit[T](t: T): Grammar[T] = Just(t)

  /**
   * Some base generators
   */
  def natGen: Grammar[Int] = new Grammar[Int] {
    def from(i: Int): Stream[Int] = i #:: from(i + 1)
    def apply() = from(0)
  }
}
