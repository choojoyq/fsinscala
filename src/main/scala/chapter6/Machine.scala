package chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

object Candy {
  def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachineSimple(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(i => State.modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    })))
    s <- State.get
  } yield (s.coins, s.candies)

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val states: List[State[Machine, Unit]] = inputs.map(i => State.modify[Machine](update(i)))
    val state: State[Machine, List[Unit]] = State.sequence(states)
    state.flatMap(_ => State.get.map(s => (s.coins, s.candies)))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(State.modify[Machine] _ compose update))
    s <- State.get
  } yield (s.coins, s.candies)
}

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def simulateMachine(inputs: List[Input]): Machine =
    inputs.foldLeft(this)((s, i) => {
      if (s.candies == 0) s
      else {
        i match {
          case Coin =>
            if (s.locked && s.candies > 0) Machine(locked = false, s.candies, s.coins + 1)
            else s
          case Turn =>
            if (!s.locked && s.candies > 0) Machine(locked = true, s.candies - 1, s.coins)
            else s
        }
      }
    })
}