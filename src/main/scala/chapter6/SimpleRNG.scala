package chapter6

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {

  def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map_2(rb)(b => f(a, b)))
  }

  def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  // report and error
  def nonNegativeLessThan2(n: Int): Rand[Int] = rng => {
    val (i, r) = nonNegativeInt(rng)
    Console.println(i)
    val mod = i % n
    if (i + (n - 1) - mod <= 0) (i, r)
    else nonNegativeLessThan2(n)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      Console.println(i)
      val mod = i % n
      if (i + (n - 1) - mod <= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (x, r1) = f(rng)
      g(x)(r1)
    }

  def ints3(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((x, xs) => map2(x, xs)(_ :: _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => rng => (List(), rng)
    case x :: xs => map2(x, sequence(xs))(_ :: _)
  }

  val int: Rand[Int] = _.nextInt

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def double2: Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1))
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - 1 % 2)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  type Rand[+A] = RNG => (A, RNG)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def go(count: Int, rng: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count <= 0)
        (xs, rng)
      else {
        val (i, r) = rng.nextInt
        go(count - 1, r, i :: xs)
      }
    }

    go(count, rng, List())
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0)
      (Nil, rng)
    else {
      val (i, r) = rng.nextInt
      val (l, r2) = ints2(count - 1)(r)
      (i :: l, r2)
    }
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    (d1, d2, d3) -> r3
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    (d, i) -> r
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    (i, d) -> r2
  }

  // +1?
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue + 1), r)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
}