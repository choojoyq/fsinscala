package chapter4

import chapter4.Option._

object Main extends App {

  println(sequence(List(Some(1), Some(2))))
  println(sequence(List(Some(1), None, Some(2))))

}
