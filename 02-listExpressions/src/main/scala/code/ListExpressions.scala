package code

import scala.annotation.tailrec

// Exercise:
//
// Using only the following methods of List:
//   List[A].head   => A
//   List[A].tail   => List[A]
//   List[A].length => Int
//   List[A].++(List[A]) => List[A]
//
// Write the following methods:
//
// - sum
//   - accepts no parameters
//   - sums all the numbers in the list
//
// - contains
//   - accepts a List[Int] and an Int value
//   - returns true if the value appears in the list
//
// - (Harder) containsAll
//   - accepts two Lists[Int] x and y
//   - returns true if all the elements of y appear in x
//
// - (Harder still) squareNumbers
//   - accepts an Int max as a parameter
//   - returns a list of the square numbers from 1 to max inclusive
//
// Tip: a square number is the result of
// multiplying another number by itself...
// 1*1, 2*2, 3*3, ...


object ListExpressions extends Exercise {
  def sum[A: Numeric](l: List[A]): A = {
    val n = implicitly[Numeric[A]]
    l.headOption.map(h => n.plus(h, sum(l.tail))) match {
      case Some(result) => result
      case None         => n.zero
    }
  }

  def contains[A: Numeric](l: List[A], value: A): Boolean = {
    val n = implicitly[Numeric[A]]
    l.headOption.forall{head => n.equiv(head, value) || contains(l.tail, value)}
  }

  def containsAll[A: Numeric](first: List[A], second: List[A]): Boolean =
    second.headOption.forall(head => contains(first, head) && containsAll(first, second.tail))

  def squareNumbers(max: Int): List[Int] = {
    require(max >= 1)

    @tailrec
    def go(curr: Int, result: List[Int]): List[Int] = {
      val currSquare = curr * curr
      if(currSquare > max) result
      else go(curr + 1, result ++ List(curr * curr))
    }

    go(1, List.empty)
  }

  // Write your methods here

  override def main(args: Array[String]): Unit = {
    // println("SUM")
    // println(sum(List(1, 2, 3)))
    // println()

    // println("CONTAINS")
    // println(contains(List(1, 2, 3), 2))
    // println(contains(List(1, 2, 3), 4))
    // println()

    // println("CONTAINS ALL")
    // println(containsAll(List(1, 2, 3), List(2, 3)))
    // println(containsAll(List(1, 2, 3), List(4, 3)))
    // println()

    // println("SQUARE NUMBERS")
    // println(squareNumbers(10))
    // println(squareNumbers(20))
    // println()
  }
}
