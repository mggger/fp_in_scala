package chapter2

object Answer {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) {
        prev
      } else {
        loop(n - 1, cur, prev + cur)
      }

    loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else go(n+1)

    go(0)
  }


  def partial[A, B, C](a: A, f:(A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =  (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}

