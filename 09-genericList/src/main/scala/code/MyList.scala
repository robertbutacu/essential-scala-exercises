package code

// Exercise 1:
//
// The code below is a copy of IntList, renamed to MyList.
//
// Make MyList generic in the type of data it holds:
// - give it a type parameter A
// - look for any erroneous Ints in the codebase and replace them with As
// - if any methods clearly don't transfer across, comment them out
// - do whatever else you have to to make it compile


// Exercise 2 (for later on):
//
// - Make MyList covariant
// - Add a ++ method to append two lists


// Exercise 3 (for even later on):
//
// - Implement foldLeft and foldRight for MyList
// - Reimplement everything else in terms of them!
// - (Optional, Very Hard) Implement foldRight in terms of foldLeft,
//   making it stack safe


// Exercise 4 (you can't believe how later on this is going to be):
//
// - Implement map on MyList
//   - For bonus points implement it in terms of foldRight
// - Can we use map to implement addToEach from IntList? Yes!
// - Reimplement add using map


// Exercise 4 (just, like, so late... don't even):
//
// - Implement map on MyList
//   - For bonus points implement it in terms of foldRight
// - Can we use map to implement addToEach from IntList? Yes!
// - Reimplement add using map


sealed trait MyList[+A] {
  def length: Int =
    this match {
      case MyPair(_, t) => 1 + t.length
      case MyNil        => 0
    }

  def contains[B >: A](item: B): Boolean =
    this match {
      case MyPair(h, t) => h == item || t.contains(item)
      case MyNil        => false
    }


  def exists[B >: A](func: B => Boolean): Boolean =
    this match {
      case MyPair(h, t) => func(h) || t.exists(func)
      case MyNil        => false
    }

  def filter[B >: A](func: B => Boolean): MyList[A] =
    this match {
      case MyPair(h, t)   =>
        if(func(h)) MyPair(h, t.filter(func)) else t.filter(func)
      case MyNil          => MyNil
    }

  def reverse: MyList[A] = {
    def loop(list: MyList[A], accum: MyList[A]): MyList[A] =
      list match {
        case MyPair(h, t)   => loop(t, MyPair(h, accum))
        case MyNil        => accum
      }

    loop(this, MyNil)
  }

  def find[B >: A](func: B => Boolean): Option[B] =
    this match {
      case MyPair(h, t) => if(func(h)) Some(h) else t.find(func)
      case MyNil        => None
    }

  def ++[B >: A](another: MyList[B]): MyList[B] = {
      this match {
        case MyNil        => another
        case MyPair(h, t) => MyPair(h, t ++ another)
      }
    }

  def foldLeft[B](accum: B)(f: (A, B) => B): B =
    this match {
      case MyNil => accum
      case MyPair(h, t) => t.foldLeft(f(h, accum))(f)
    }

  def foldRight[B](accum: B)(f: (A, B) => B): B = {
    ???
  }

  def map[B](f: A => B): MyList[B] =
    this match {
      case MyNil => MyNil
      case MyPair(h, t) => MyPair(f(h), t.map(f))
    }

  def flatMap[B, F[_]](f: A => F[B]): MyList[B] = {
    ???
  }


  // - Implement foldLeft and foldRight for MyList

}

case class MyPair[A](head: A, tail: MyList[A]) extends MyList[A]

case object MyNil extends MyList[Nothing]
