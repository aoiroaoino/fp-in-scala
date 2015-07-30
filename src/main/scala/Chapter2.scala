package fpinscala

object Chapter2 {

  def fib(n: Int): Int = {
    // def _fib(n) = if (n <= 0) 0 else if (n == 1) 1 else _fib(n - 2) + _fib(n - 1)
    @annotation.tailrec
    def loop(n: Int, a: Int, b: Int): Int = {
      if (n < 3)
        a + b
      else
        loop(n - 1, b, a + b)
    }
    if (n <= 0) 0 else if (n == 1) 1 else loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A], orderd: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, acc: Boolean): Boolean = {
      if (n <= 0)
        acc
      else
        loop(n - 1, acc && orderd(as(n - 1), as(n)))
    }
    loop(as.length - 1, true)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}

