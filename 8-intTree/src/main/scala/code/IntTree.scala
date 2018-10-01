package code

// (Optional, Harder) Exercise: Design a data type for a binary tree of integers.
//
// Tip: Implement the methods below one at a time,
// checking them against the unit tests before moving on to the next
//
// Implement the following methods:
// - size — the number of nodes in the tree
// - addToEach — add a number to each node in the tree
// - sum — add together all of the nodes in the tree
// - exists
//   - takes a predicate as a parameter
//   - returns true if it applies to any node in the tree
// - filter
//   - takes a predicate as a parameter
//   - returns a tree of all nodes for which the predicate returns true
// - find
//   - takes a predicate as a parameter
//   - returns the first node (left to right) for which it returns true
// - (Optional) toList — linearise a list of all the nodes
// - (Optional) depth — the maximum distance from the root to a leaf
// - (Optional) width — number of nodes in the widest horizontal layer

sealed trait IntTree {
  def size: Int =
    this match {
      case IntNode(_, l, r) => 1 + l.size + r.size
      case IntLeaf          => 0
    }

  def addToEach(num: Int): IntTree =
    this match {
      case IntNode(n, l, r) => IntNode(n + num, l.addToEach(num), r.addToEach(num))
      case IntLeaf          => IntLeaf
    }

  def sum: Int =
    this match {
      case IntNode(n, l, r) => n + l.sum + r.sum
      case IntLeaf          => 0
    }

  def exists(func: Int => Boolean): Boolean =
    this match {
      case IntNode(n, l, r) => func(n) || l.exists(func) || r.exists(func)
      case IntLeaf          => false
    }

  def find(func: Int => Boolean): Option[Int] =
    this match {
      case IntNode(n, l, r) => Some(n).filter(func) orElse l.find(func) orElse r.find(func)
      case IntLeaf          => None
    }

  def toList: List[Int] =
    this match {
      case IntNode(n, l, r) => l.toList ++ List(n) ++ r.toList
      case IntLeaf          => Nil
    }

  def depth: Int =
    this match {
      case IntNode(_, l, r) => 1 + math.max(l.depth, r.depth)
      case IntLeaf          => 0
    }

  def width: Int = {
    val temp = widths
    if(temp.isEmpty) 0 else temp.max
  }

  def widths: List[Int] =
    this match {
      case IntNode(_, l, r) =>
        val lWidths = l.widths
        val rWidths = r.widths
        val padLength = math.max(lWidths.length, rWidths.length)
        val lPadded = lWidths.padTo(padLength, 0)
        val rPadded = rWidths.padTo(padLength, 0)
        1 :: lPadded.zip(rPadded).map { case (x, y) => x + y }

      case IntLeaf          => Nil
    }

  def filter(func: Int => Boolean): IntTree =
    this match {
      case IntNode(n, l, r) => if(func(n)) IntNode(n, l.filter(func), r.filter(func)) else l.filter(func) ++ r.filter(func)
      case IntLeaf          => IntLeaf
    }

  def ++(that: IntTree): IntTree =
    this match {
      case IntNode(n, l, r) => IntNode(n, l, r ++ that)
      case IntLeaf          => that
    }
}

case class IntNode(n: Int, l: IntTree, r: IntTree) extends IntTree

case object IntLeaf extends IntTree
