package chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("tail is Nil")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], head: A): List[A] = l match {
    case Nil => throw new NoSuchElementException("list is Nil")
    case Cons(_, xs) => Cons(head, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }

  // use buff to prevent stack overflow
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("init is Nil")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(l)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def length[A](xs: List[A]): Int = foldRight(xs, 0)((_, y) => y + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length2[A](xs: List[A]): Int = foldLeft(xs, 0)((x, _) => x + 1)

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil: List[A])((x, y) => Cons(y, x))

  def append[A](xs: List[A], el: A): List[A] = foldRight(xs, List(el))(Cons(_, _))

  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys)((ys, x) => Cons(ys, x))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((acc, h) => f(h, acc))

  // TODO review
  def foldRightViaFoldLeft2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  def transformIntegers(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, transformIntegers(xs))
    }

  def transformIntegers2(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((h, t) => Cons(f(h), t))

  def map2[A, B](as: List[A])(f: A => B): List[B] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[B]

    def go(l: List[A]): Unit =
      l match {
        case Nil => ()
        case Cons(x, xs) => buf += f(x); go(xs)
      }

    go(as)
    List(buf.toList: _*)
  }

  // filter 2 same as map
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((h, t) => append(f(h), t))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(e => if (f(e)) List(e) else List())

  def sumLists(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumLists(xs, ys))
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def go(sup2: List[A], sub2: List[A], b: Boolean): Boolean = (sup2, sub2) match {
      case (Cons(_, _), Nil) => b
      case (Nil, Cons(_, _)) => false
      case (Nil, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) => if (x == y) go(xs, ys, true) else go(xs, sub, false)
    }

    go(sup, sub, b = false)
  }

}
