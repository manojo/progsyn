/*package gen

import scala.collection.immutable.Queue

trait Generators {

  abstract class Gen[+T] {
    def parent: Option[Gen[_]]
    def children: List[(Gen[_], Gen[_])]

    def isBaseGen: Boolean

    def values: Stream[T]

    def backPropagate(
      ts: Stream[T],
      queue: Queue[Gen[_]]): Stream[T]
  }

  class IntGen extends Gen[Int] {
    def children = Nil

    def from(i: Int): Stream[Int] = i #:: from(i + 1)
    def values: Stream[Int] = from(0)

    def backPropagate(
      ts: Stream[T],

    )
  }
}
*/
