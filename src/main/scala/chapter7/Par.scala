package chapter7

import java.util.concurrent._

class Par[A] {

}

object Par {

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  def flatMap2[A, B](z: Par[A])(f: A => Par[B]): Par[B] =
    join(map(z)(f))

  def join2[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(pa => pa)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => a(es).get()(es)

  def flatMap[A, B](z: Par[A])(f: A => Par[B]): Par[B] =
    es => f(z(es).get())(es)

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key)(choices(_))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(b => if (b) t else f)

  def choiceN[A](n: Par[Int])(choises: List[Par[A]]): Par[A] =
    flatMap(n)(choises(_))

  def reduce[A, B](la: List[A], z: B)(fs: A => B)(f: (B, B) => B): Par[B] =
    if (la.isEmpty) unit(z)
    else if (la.size == 1) unit(fs(la.head))
    else {
      val (l, r) = la.splitAt(la.length / 2)
      Par.map2(reduce(l, z)(fs)(f), reduce(r, z)(fs)(f))(f)
    }

  def sum2(ints: List[Int]): Par[Int] =
    reduce(ints, 0)(a => a)(_ + _)

  def wordCount2(ls: List[String]): Par[Int] =
    reduce(ls, 0)(w => w.length)(_ + _)

  def max2(li: List[Int]): Par[Int] =
    reduce(li, 0)(i => i)((a, b) => math.max(a, b))

  def wordCount(ls: List[String]): Par[Int] =
    if (ls.size <= 1)
      Par.unit(ls.headOption.map(_.length).getOrElse(0))
    else {
      val (l, r) = ls.splitAt(ls.length / 2)
      Par.map2(wordCount(l), wordCount(r))(_ + _)
    }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence_simple[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((pa, l) => map2(pa, l)(_ :: _))

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  type Par[A] = ExecutorService => java.util.concurrent.Future[A]

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends java.util.concurrent.Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))((l, r) => math.max(l, r))
    }
}
