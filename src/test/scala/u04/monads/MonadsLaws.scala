package scala.u04.monads

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}
import u04.monads.Monads.Monad

abstract class MonadsLaws[F[_]](name: String)(using monad: Monad[F]) extends Properties(name):

  def equivalent[A](m1: F[A], m2: F[A]): Boolean

  def leftIdentity[A, B](a: A, f: A => F[B]): Boolean =
    val lhs = monad.flatMap(monad.unit(a))(f)
    val rhs = f(a)
    equivalent(lhs, rhs)

  def rightIdentity[A, B](m: F[A]): Boolean =
    equivalent(m.flatMap(monad.unit), m)

  def associativity[A, B, C](m: F[A], f: A => F[B], g: B => F[C]): Boolean =
    val lhs = m.flatMap(f).flatMap(g)
    val rhs = monad.flatMap(m)((x: A) => monad.flatMap(f(x))(g))
    equivalent(lhs, rhs)
