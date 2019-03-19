package chapter8

import chapter6.{RNG, SimpleRNG, State}

case class SGen[+A](g: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen(g(_) map f)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen(n => g(n).flatMap(
      f(_).g(n)
    ))
  }
}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => Gen.listOfN(n, g))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => Gen.listOfN(n max 1, g))
  }

}

case class Gen[+A](sample: State[RNG, A]) {

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))

  def unsized: SGen[A] =
    SGen(_ => this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(gb.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))
}

object Gen {

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(SimpleRNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def boolean: Gen[Boolean] =
    Gen(State(SimpleRNG.nonNegativeInt).map(i => i % 2 == 0))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(SimpleRNG.nonNegativeInt).map(i => start + i % (stopExclusive - start)))

}
