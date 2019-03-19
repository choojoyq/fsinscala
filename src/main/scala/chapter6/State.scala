package chapter6

import chapter6.State._

case class State[S, +A](run: S => (A, S)) {

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {

  def test: State[RNG, List[Int]] = {
    for {
      x <- State(SimpleRNG.int)
      y <- State(SimpleRNG.int)
      xs <- State(SimpleRNG.ints(x))
    } yield xs.map(_ % y)
  }

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def get[S]: State[S, S] = State(s => (s, s))

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((sa, sb) => sa.map2(sb)(_ :: _))

  def sequence2[S, A](sas: List[State[S, A]]): State[S, List[A]] = {

    def go(sas: List[State[S, A]], s: S, acc: List[A]): (List[A], S) = {
      sas match {
        case Nil => (acc.reverse, s)
        case x :: xs =>
          val (a, s1) = x.run(s)
          go(xs, s1, a :: acc)
      }
    }

    State((s: S) => go(sas, s, List()))
  }
}
