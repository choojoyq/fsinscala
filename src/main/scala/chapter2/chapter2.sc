import scala.annotation.tailrec


val list = List(1, 2, 3, 4, 5)

list.foldLeft(0)(_ - _)
list.foldRight(0)(_ - _)

def fib(n: Int): Int = {
  @tailrec
  def goo(prev: Int, next: Int, step: Int): Int = {
    if (step == 0)
      prev
    else goo(next, prev + next, step - 1)
  }

  goo(0, 1, n)
}

fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)

def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  a: A => (b: B) => f(a, b)
}

def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a: A => f(g(a))
}