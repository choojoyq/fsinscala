package chapter5

/**
  * @author Nikita Gorbachevski
  */
object Main extends App {

  println(Stream(1, 2, 3, 4, 5).toList)
  println(Stream(1, 2, 3, 4, 5).take(2).toList)
  println(Stream(1, 2, 3, 4, 5).drop(2).toList)
  println(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList)
  println(Stream(1, 2, 3, 4, 5).headOption)
  println(Stream().headOption)
  println(Stream(1, 2, 3, 4, 5).map(_ + 1).toList)
  println(Stream(1, 2, 3, 4, 5).append(Stream(6, 7)).toList)

  println(Stream.constant(1).take(3).toList)
  println(Stream.constant2(1).take(3).toList)
  println(Stream.constant3(1).take(3).toList)
  println(Stream.fibs.take(10).toList)
  println(Stream.fibs2.take(10).toList)
  println(Stream.from(1).take(5).toList)
  println(Stream.from2(1).take(5).toList)

  println(Stream(1, 2, 3).tails.toList.map(_.toList))

  println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)

}
