package chapter8

import chapter7.Par

object Main extends App {

  val pint2 = Gen.choose(-10, 10).listOfN(Gen.choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }))
  val p5 =
    Prop.forAllPar(pint2) { i => Par.equal(Par.fork(i), i) }
//  Prop.run(p5)

  val pint = Gen.choose(0, 10) map (Par.unit(_))
  val p4 =
    Prop.forAllPar(pint)(n => Par.equal(Par.map(n)(y => y), n))
  Prop.run(p4)


  val p3 = Prop.checkPar {
    Par.equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }
  Prop.run(p3)

  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  Prop.run(maxProp)

  val sortedProp = Prop.forAll(SGen.listOf(smallInt)) { ns =>
    val nss = ns.sorted
    (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
      case (a, b) => a > b
    }) && nss.forall(ns.contains(_)) && ns.forall(nss.contains(_))
  }
  Prop.run(sortedProp)
}
