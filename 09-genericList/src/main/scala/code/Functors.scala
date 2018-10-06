package code

import scala.util.{Success, Try}

object Functors extends App {
  trait Functor[F[_]]{
    def map[B, A](src: F[A])(func: A => B): F[B]
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[B, A](src: List[A])(func: A => B): List[B] = src.map(func)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[B, A](src: Option[A])(func: A => B): Option[B] = src.map(func)
  }

  implicit val tryFunctor: Functor[Try] = new Functor[Try] {
    def map[B, A](src: Try[A])(func: A => B): Try[B] = src.map(func)
  }

  implicit val myListFunctor: Functor[MyList] = new Functor[MyList] {
    def map[B, A](src: MyList[A])(func: A => B): MyList[B] = src.map(func)
  }

  def mulN[F[_], B](src: F[B], times: B)(implicit f: Functor[F], n: Numeric[B]): F[B] =
    f.map(src)(input => n.times(input, times))


  println(mulN(Option(2), 2))

  def add[F[_]](a: F[Int], b: F[Int])(implicit m: Monad[F]) = {
    a.flatMap(v => b.map(w => v + w))
  }
}
