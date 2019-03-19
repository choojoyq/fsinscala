package chapter2

import scala.annotation.tailrec

/**
  * @author Nikita Gorbachevski
  */
object Chapter2 extends App {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }

  val ord = (a: Int, b: Int) => a > b

  println(isSorted[Int](Array(), ord))
  println(isSorted[Int](Array(1), ord))
  println(isSorted[Int](Array(1, 2), ord))
  println(isSorted[Int](Array(2, 1), ord))
  println(isSorted[Int](Array(1, 2, 3), ord))
  println(isSorted[Int](Array(1, 3, 2), ord))
  println(isSorted[Int](Array(3, 2, 1), ord))

}
