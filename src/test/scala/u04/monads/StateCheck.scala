package scala.u04.monads

import u04.monads.States.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

object StateCheck extends MonadsLaws[[A] =>> State[Int, A]]("StateLaws"):

  val initialState: Int = 0

  def equivalent[A](m1: State[Int, A], m2: State[Int, A]): Boolean =
    m1.apply(initialState) == m2.apply(initialState)

  given Arbitrary[State[Int, Int]] = Arbitrary:
    Arbitrary.arbitrary[Int].map(value => State(s => (s + 1, value)))

  given Arbitrary[Int => State[Int, Int]] = Arbitrary:
    Gen.oneOf[Int => State[Int, Int]](
      v => State(s => (s, v * 2)),
      v => State(s => (s + v, v))
    )

  property("Left Identity") = forAll((a: Int, f: Int => State[Int, Int]) => leftIdentity(a, f))
  property("Right Identity") = forAll((m: State[Int, Int]) => rightIdentity(m))
  property("Associativity") = forAll((m: State[Int, Int], f: Int => State[Int, Int], g: Int => State[Int, Int]) => associativity(m, f, g))