package fpinscala.chapter3

// import scala.collection.immutable.{:: => Cons}

trait List[+A]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  // exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  assert(x == 3)


  // exercise 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(x, xs) => xs
      case Nil         => error("unsupported operation")
    }


  // exercise 3.3
  def setHead[A](v: A, l: List[A]): List[A] =
    l match {
      case Cons(_, xs) => Cons(v, xs)
      case Nil         => error("unsupported operation")
    }


  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0 || l == Nil) l else drop(tail(l), n - 1)


  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
      case other                 => other
    }


  // exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs)  => Cons(x, init(xs))
      case Nil          => error("unsupported operation")
    }


  // exercise 3.7
  def product0(l: List[Double]): Double =
    error("impossible")


  // exercise 3.8
  val l38 = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  assert(l38 == List(1, 2, 3))
  assert(l38 == Cons(1, Cons(2, Cons(3, Nil))))


  // exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, i) => 1 + i)


  // exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // exercise 3.11
  def suml(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productl(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def lengthl[A](l: List[A]): Int =
    foldLeft(l, 0)((i, _) => i + 1)


  // exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))


  // exercise 3.13
  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    ??? // todo ...

  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    ??? // todo ...


  // exercise 3.14
  def appendr[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  def appendl[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l1), l2)((x, y) => Cons(y, x))


  // exercise 3.15
  def multiAppend[A](ls: List[A]*): List[A] =
    foldLeft(List(ls: _*), Nil: List[A])(appendr(_, _))
}
