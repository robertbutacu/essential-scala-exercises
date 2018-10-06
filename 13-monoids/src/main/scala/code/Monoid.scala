package code

// Exercise 1:
//
// Create a type class "Monoid[A]" with two abstract methods:
//
// - a method "combine" that
//   - accepts two parameters of type A
//   - returns a single value of type A
//   - the intention is that implementations will provide some kind of "adding" behaviour
//
// - a method "empty" that
//   - accepts no parameters
//   - returns a value of type A
//   - the intention is that implementations will provide some kind of "zero" value
//     that provides an identity for "combine" (i.e. X combine 0 == X)
//
// Create a companion object for Monoid containing instances for several types.
// In each case decide what the implementation of empty and combine should be:
//
// - Int
// - String
// - (Optional) Double
// - (Optional) List
//
// Tip: If you find the boilerplate for writing anonymous classes to be too much,
// you may want to introduce a helper method that you can use in your implementations.


// Exercise 2 (for later):
//
// Try summoning a few instances using "implicitly":
//
//   implicitly[Monoid[Int]]
//   implicitly[Monoid[String]]
//   implicitly[Monoid[Double]]
//   implicitly[Monoid[List[Int]]]
//
// Let's make this more convenient.
// Create a "summoner" method in the Monoid companion object:
//
// - it should be called "apply"
// - it should take one implicit parameter of type Monoid[A]
// - it should return the parameter
//
// This is a pattern you'll see in many libraries.
// Now you can summon instances by type without knowing their names, e.g.:
//
//   Monoid[Int]
//   Monoid[String]
//   Monoid[Double]
//   Monoid[List[Int]]
//
//
// Create another object called MonoidSyntax containing two methods:
//
// - empty
//   - accepts an implicit Monoid[A]
//   - returns the result of calling its empty method
//
// - combine
//   - accepts two instances of a type A
//   - accepts an implicit Monoid[A]
//   - uses the Monoid to combine the instances
//
// Now you can do:
//
//   import MonoidSyntax._
//   combine(1, 2)
//   combine("foo", "bar")
//
// And so on. We'll add more to this object later.


// Exercise 3 (for so much later than that):
//
// Create derived instances of Monoid for other types:
//
// - Option[A] -- this should depend on the instance for A
// - (A, B) -- this should depend on the instances for A and B


// Exercise 4 (for even later still):
//
// Create another object, MonoidSyntax,
// containing an implicit class MonoidOps[A]
// that provides the following extension method
// to any value of an arbitrary type A:
//
// - a method |+| that
//   - accepts a second value of type A
//   - and an implicit Monoid[A]
//   - and adds the two values together
//
// Create another implicit class in MonoidSyntax, MonoidListOps[A],
// that provides extension methods to any value of type List[A]:
//
// - a method combineAll that
//   - accepts an implicit Monoid[A]
//   - folds down the list adding all the elements together


trait Monoid[A] {
  def empty: A
  def combine(a: A, b: A): A
}

object Monoid {
  def create[A](e: A)(c: (A, A) => A): Monoid[A] = new Monoid[A] {
    override def empty: A = e

    override def combine(a: A, b: A): A = c(a, b)
  }

  implicit def numericMonoid[A: Numeric](implicit n: Numeric[A]): Monoid[A] = create(n.zero)(n.plus)

  implicit def stringMonoid: Monoid[String] = create[String]("")(_ + _)

  implicit def listMonoid[A]: Monoid[List[A]] = create(List.empty[A])(_ ++ _)

  implicit def optionMonoid[A](implicit m: Monoid[A]): Monoid[Option[A]] = create[Option[A]](None){(a: Option[A], b: Option[A]) =>
    for {
      v1 <- a
      v2 <- b
    } yield m.combine(v1, v2)
  }

  def combineAll[A](list: List[A])(implicit m: Monoid[A]) =
    list.foldLeft(m.empty)(m.combine)
}

object MonoidExercise extends Exercise {
   println("CALLING EMPTY DIRECTLY")
   println(Monoid.numericMonoid[Int].empty)
   println(Monoid.stringMonoid.empty)
   println(Monoid.numericMonoid[Double].empty)
   println(Monoid.listMonoid[Int].empty)
   println()

   println("CALLING COMBINE DIRECTLY")
   println(Monoid.numericMonoid[Int].combine(1, 2))
   println(Monoid.stringMonoid.combine("foo", "bar"))
   println(Monoid.numericMonoid[Double].combine(1.2, 3.4))
   println(Monoid.optionMonoid[Double].combine(Some(1.2), Some(3.4)))
   println(Monoid.listMonoid[Int].combine(List(1, 2), List(3, 4)))
   println()

   println("CALLING EMPTY ON A SUMMONED INSTANCE")
   println(implicitly[Monoid[Int]].empty)
   println(implicitly[Monoid[String]].empty)
   println(implicitly[Monoid[Double]].empty)
   //println(Monoid.apply[List[Int]].empty)
   println()

//   println("CALLING COMBINE ON A SUMMONED INSTANCE")
//   println(Monoid[Int].combine(1, 2))
//   println(Monoid[String].combine("foo", "bar"))
//   println(Monoid[Double].combine(1.2, 3.4))
//   println(Monoid[List[Int]].combine(List(1, 2), List(3, 4)))
//   println(Monoid[Option[Int]].combine(Option(123), Option(456)))
//   println(Monoid[(String, Int)].combine(("A", 1), ("B", 2)))
//   println()
//
//   import Monoid._
//
//   println("USING MONOIDSYNTAX EMPTY")
//   println(empty[Int])
//   println(empty[String])
//   println(empty[Double])
//   println(empty[List[Int]])
//   println(empty[Option[Int]])
//   println(empty[(String, Int)])
//   println()
//
//   println("USING MONOIDSYNTAX COMBINE")
//   println(combine(1, 2))
//   println(combine("foo", "bar"))
//   println(combine(1.2, 3.4))
//   println(combine(List(1, 2), List(3, 4)))
//   println(combine(Option(123), Option(456)))
//   println(combine(("A", 1), ("B", 2)))
//   println()
//
//   println("USING MONOIDSYNTAX |+|")
//   println(1 |+| 2)
//   println("foo" |+| "bar")
//   println(1.2 |+| 3.4)
//   println(List(1, 2) |+| List(3, 4))
//   println(Option(123) |+| Option(456))
//   println(("A", 1) |+| (("B", 2)))
//   println()
//
//   println("USING MONOIDSYNTAX COMBINEALL")
//   println(List(1, 2, 3).combineAll)
//   println(List("foo", "bar", "baz").combineAll)
//   println(List(1.2, 3.4, 5.6).combineAll)
//   println(List(Some(123), None, Some(456)).combineAll)
//   println(List(("A", 1), ("B", 2), ("C", 3)).combineAll)
//   println()
}