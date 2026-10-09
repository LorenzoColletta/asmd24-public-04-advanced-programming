package scala.u04.monads

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import u04.monads.Optionals.Optional
import u04.monads.Optionals.Optional.Just


object OptionalCheck extends MonadsLaws[Optional]("Optional"):

  @Override
  override def equivalent[A](m1: Optional[A], m2: Optional[A]): Boolean = m1 == m2

  def optionalGen[A: Arbitrary](): Gen[Optional[A]] = arbitrary[A].map(Just(_))

  given intOptionalArbitrary: Arbitrary[Optional[Int]] = Arbitrary(optionalGen[Int]())
  given mapperArbitrary: Arbitrary[Int => Optional[Int]] = Arbitrary(Gen.oneOf[Int => Optional[Int]](
    x => Just(x+1),
    x => Just(x*2),
    x => Just(x*x)))

  property("Left Identity") =
    forAll: (o: Int, f: Int => Optional[Int]) =>
      leftIdentity(o, f)

  property("Right Identity") =
    forAll: (m: Optional[Int]) =>
      rightIdentity(m)

  property("Associativity") =
    forAll: (m: Optional[Int], f: Int => Optional[Int], g: Int => Optional[Int]) =>
      associativity(m, f, g)

