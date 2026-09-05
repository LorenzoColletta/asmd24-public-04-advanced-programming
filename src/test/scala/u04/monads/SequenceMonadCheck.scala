package scala.u04.monads

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import u04.monads.Sequences.*
import u04.monads.Sequences.Sequence
import u04.monads.Sequences.Sequence.{Cons, Nil}
import u04.monads.Monads.*

import scala.u04.monads.SequenceCheck.sequenceGen

object SequenceMonadCheck extends MonadsLaws[Sequence]("SequenceMonadLaws"):
  import Sequence.*
  
  def equivalent[A](m1: Sequence[A], m2: Sequence[A]): Boolean =
    m1 == m2

  def sequenceGen[A: Arbitrary](): Gen[Sequence[A]] = for
    i <- arbitrary[A]
    b <- Gen.prob(0.8)
    s <- if b then sequenceGen().map(s2 => Cons(i, s2)) else Gen.const(Nil())
  yield s
  
  given Arbitrary[Sequence[Int]] = Arbitrary(sequenceGen[Int]())

  given Arbitrary[Int => Sequence[Int]] = Arbitrary(Gen.oneOf[Int => Sequence[Int]](
    x => Cons(x+1, Nil()),
    x => Cons(x*2, Nil()),
    x => Cons(x*x, Nil())))

  property("Left Identity") = forAll((a: Int, f: Int => Sequence[Int]) => leftIdentity(a, f))
  property("Right Identity") = forAll((m: Sequence[Int]) => rightIdentity(m))
  property("Associativity") = forAll((m: Sequence[Int], f: Int => Sequence[Int], g: Int => Sequence[Int]) => associativity(m, f, g))