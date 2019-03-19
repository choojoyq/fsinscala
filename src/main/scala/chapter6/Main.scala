package chapter6

import chapter6.SimpleRNG._

object Main extends App {
  val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  val (n2, rng3) = rng2.nextInt

  println(double(new SimpleRNG(Int.MaxValue)))
  println(ints(5)(rng))
  println(ints2(5)(rng))
  println(ints3(5)(rng))

//  State.test.run(new SimpleRNG(200))

  val machine = Machine(true, 10, 5)
  println(machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)))
  val r: State[Machine, (Int, Int)] = Candy.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
  println(r.run(machine))
}
