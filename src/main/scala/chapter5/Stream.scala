package chapter5

import chapter5.Stream._

sealed trait Stream[+A] {

  // it's possible to pass z as the second value!
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(cons(z, empty))((e, s) => {
      lazy val st = s
      cons(f(e, st.headOption.get), st)
    })

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some(Cons(h, t), t())
      case Empty => None
    } append Stream(empty)

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll { case (l, r) => l == r }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())) -> (t1() -> t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())) -> (empty -> t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None) -> (t1() -> empty))
      case (Empty, Empty) => None
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case (Cons(h, _), 1) => Some(h(), (empty, 0))
      case _ => None
    }

  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def headOption2: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def ones2: Stream[Int] =
    unfold(1)(_ => Some(1, 1))

  def ones: Stream[Int] = Stream.cons(1, ones)

  def constant3[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def from2(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def fibs2: Stream[Int] =
    unfold((0, 1)) { case (p, n) => Some(p, (n, p + n)) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def fibs: Stream[Int] = {
    def go(prev: Int, next: Int): Stream[Int] =
      cons(prev, go(next, prev + next))

    go(0, 1)
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
