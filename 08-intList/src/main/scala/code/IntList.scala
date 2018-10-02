package code

import scala.annotation.tailrec

// Exercise:
//
// Implement an ADT called IntList!
//
// Tip: Implement the methods below one at a time,
// checking them against the unit tests before moving on to the next
//
// Tip: Don't worry about making your methods tail recursive
// (unless you get to the end and have time to spare)
//
// Implement the following methods:
//
// - length
//    - takes no parameters
//    - returns the length of the list
//
//  - contains
//    - takes an Int parameter
//    - checks whether the Int is in the list
//
//  - addToEach
//    - takes an Int parameter n
//    - adds n to every element in the list, returning a new list
//
//  - sum
//    - takes no parameters
//    - adds up all the elements in the list
//
//  - exists
//    - takes a predicate as a parameter
//    - returns true if the predicate applies to any item in the list
//
//  - filter
//    - takes a predicate as a parameter
//    - returns a list of all items for which the predicate returns true
//
//  - (Optional) reverse
//    - takes no parameters
//    - reverses the list
//
//  - (Optional) find
//    - takes a predicate as a parameter
//    - returns the first item for which it returns true
//    - Note:
//      - how can you handle the case where we can't find the relevant item?
//      - either use an Option to wrap the result, or introduce your own IntOption ADT
//
//  - (Optional, Harder) — make as many of your methods tail recursive as possible
//    - Fork your code by copying the file if you do this!
//    - We'll be working with this code again and, ultimately, it'll be easier to not have the extra complexity.


sealed trait IntList {
  def length: Int = {
    this match {
      case IntPair(_, t) => 1 + t.length
      case IntNil => 0
    }
  }

  @tailrec
  final def contains(elem: Int): Boolean = {
    this match {
      case IntPair(h, t) => h == elem || t.contains(elem)
      case IntNil        => false
    }
  }

  def addToEach(n: Int): IntList = {
    this match {
      case IntPair(h, t) => IntPair(h + n, t.addToEach(n))
      case IntNil        => IntNil
    }
  }

  def sum(): Int = {
    this match {
      case IntPair(h, t) => h + t.sum()
      case IntNil        => 0
    }
  }

  @tailrec
  final def exists(f: Int => Boolean): Boolean = {
    this match {
      case IntPair(h, t) => f(h) || t.exists(f)
      case IntNil        => false
    }
  }

  def filter(f: Int => Boolean): IntList = {
    this match {
      case IntPair(h, t) => if(f(h)) IntPair(h, t.filter(f)) else t.filter(f)
      case IntNil        => IntNil
    }
  }

  def addElem(n: Int): IntList = {
    this match {
      case IntPair(h, t) => IntPair(h, t.addElem(n))
      case IntNil        => IntPair(n, IntNil)
    }
  }

  def reverse: IntList = {
    this match {
      case IntPair(h, t) => t.reverse.addElem(h)
      case IntNil        => IntNil
    }
  }

  def reverseOptimized: IntList = {
    @tailrec
    def go(curr: IntList, acc: IntList): IntList = {
      curr match {
        case IntNil => acc
        case IntPair(h, t) => go(t, IntPair(h, acc))
      }
    }

    go(this, IntNil)
  }

  @tailrec
  final def find(f: Int => Boolean): Option[Int] = {
    this match {
      case IntPair(h, t) => if(f(h)) Some(h) else t.find(f)
      case IntNil        => None
    }
  }
}

case class IntPair(head: Int, tail: IntList) extends IntList

case object IntNil extends IntList