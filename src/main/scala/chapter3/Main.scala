package chapter3

/**
  * @author Nikita Gorbachevski
  */
object Main extends App {

  import chapter3.List._
  import chapter3.Tree._

  val xs: List[Int] = List(1, 2, 3, 4, 5)
  println(dropWhile(xs)(x => x < 4))

  println(foldRight(List(1, 2, 3, 4, 5), Nil: List[Int])(Cons(_, _)))
  println(foldRightViaFoldLeft(List(1, 2, 3, 4, 5), Nil: List[Int])(Cons(_, _)))

  println(foldLeft(List(1, 2, 3, 4, 5), Nil: List[Int])((acc, h) => Cons(h, acc)))

  println(length(List(1, 2, 3, 4)))

  println(length2(List("a", "b", "c")))

  println(reverse(List(1, 2, 3, 4, 5)))

  println(append(List(1, 2, 3, 4), 5))

  println(transformIntegers(List(1, 2, 3)))
  println(transformIntegers2(List(1, 2, 3)))
  println(doubleToString(List(1.1, 2.2, 3.3)))
  println(map(List(1, 2, 3))(_ + 1))
  println(map2(List(1, 2, 3))(_ + 1))
  println(filter(List(1, 2, 3, 4))(_ % 2 == 0))
  println(flatMap(List(1, 2, 3))(e => List(e, e)))
  println(filterViaFlatMap(List(1, 2, 3, 4))(_ % 2 == 0))
  println(sumLists(List(1, 2, 3, 4), List(4, 5, 6, 7, 8)))

  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(1)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(4)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(1, 2, 3)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(4, 5)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(4, 5, 6)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(4, 6)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(1, 4)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(1, 2, 5)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(1, 2, 5, 6)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(4, 5, 6, 7)))

  println(size(Branch(Leaf(1), Leaf(2))))
  println(maximum(Branch(Branch(Leaf(3), Leaf(2)), Branch(Leaf(1), Leaf(4)))))
  println(depth(Branch(Branch(Leaf(3), Leaf(2)), Leaf(3))))
  println(mapTr(Branch(Branch(Leaf(3), Leaf(2)), Leaf(3)))(i => i * 2))

  println(size2(Branch(Leaf(1), Leaf(2))))
  println(maximum2(Branch(Branch(Leaf(3), Leaf(2)), Branch(Leaf(1), Leaf(4)))))
  println(depth2(Branch(Branch(Leaf(3), Leaf(2)), Leaf(3))))
  println(mapTr2(Branch(Branch(Leaf(3), Leaf(2)), Leaf(3)))(i => i * 2))
}
