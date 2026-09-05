package scala.u04.monads
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import u04.monads.IOs.*
import u04.monads.Monads.*

object IOCheck extends MonadsLaws[IO]("IOLaws"):
  import IO.*

  // Esecuzione dell'IO per confrontarne il risultato[cite: 10]
  def equivalent[A](m1: IO[A], m2: IO[A]): Boolean =
    m1.exec() == m2.exec()

  // Generatori basati su computazioni pure[cite: 10]
  given Arbitrary[IO[Int]] = Arbitrary:
    Arbitrary.arbitrary[Int].map(v => compute(v))

  given Arbitrary[Int => IO[Int]] = Arbitrary:
    Gen.oneOf[Int => IO[Int]](
      x => compute(x * 2),
      x => compute(x + 10)
    )

  // Registrazione delle property
  property("Left Identity") = forAll((a: Int, f: Int => IO[Int]) => leftIdentity(a, f))
  property("Right Identity") = forAll((m: IO[Int]) => rightIdentity(m))
  property("Associativity") = forAll((m: IO[Int], f: Int => IO[Int], g: Int => IO[Int]) => associativity(m, f, g))