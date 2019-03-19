package chapter3


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tr: Tree[A]): Int = tr match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def maximum(tr: Tree[Int]): Int = tr match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }

  def depth[A](tr: Tree[A]): Int = tr match {
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case Leaf(_) => 1
  }

  def mapTr[A, B](tr: Tree[A])(f: A => B): Tree[B] = tr match {
    case Branch(l, r) => Branch(mapTr(l)(f), mapTr(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  def fold[A, B](tr: Tree[A])(f: A => B)(combine: (B, B) => B): B = tr match {
    case Branch(l, r) => combine(fold(l)(f)(combine), fold(r)(f)(combine))
    case Leaf(v) => f(v)
  }

  def size2[A](tr: Tree[A]): Int = fold(tr)(_ => 1)(1 + _ + _)

  def maximum2(tr: Tree[Int]): Int = fold(tr)(v => v)(_ max _)

  def depth2[A](tr: Tree[A]): Int = fold(tr)(_ => 1)((l: Int, r: Int) => 1 + (r max l))

  def mapTr2[A, B](tr: Tree[A])(f: A => B): Tree[B] = fold(tr)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}