package cats
package laws
package discipline

import cats.functor.Invariant
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

 class InvariantTestImplicits[F[_], A, B, C](implicit
  val arbA: Arbitrary[A],
  val arbB: Arbitrary[B],
  val arbC: Arbitrary[C],
  val arbFA: Arbitrary[F[A]],
  val eqFA: Eq[F[A]],
  val eqFC: Eq[F[C]]
)

object InvariantTestImplicits {
  implicit def materialize[F[_], A, B, C](implicit
    arbA: Arbitrary[A],
    arbB: Arbitrary[B],
    arbC: Arbitrary[C],
    arbFA: Arbitrary[F[A]],
    eqFA: Eq[F[A]],
    eqFC: Eq[F[C]]
  ): InvariantTestImplicits[F, A, B, C] =
    new InvariantTestImplicits()(arbA, arbB, arbC, arbFA, eqFA, eqFC)
}

trait InvariantTests[F[_]] extends Laws {
  def laws: InvariantLaws[F]

  def invariant[A, B, C](implicit implicits: InvariantTestImplicits[F, A, B, C]): RuleSet = {
    import implicits._
    new DefaultRuleSet(
      name = "invariant",
      parent = None,
      "invariant identity" -> forAll(laws.invariantIdentity[A] _),
      "invariant composition" -> forAll(laws.invariantComposition[A, B, C] _))
  }
}

object InvariantTests {
  def apply[F[_]: Invariant]: InvariantTests[F] =
    new InvariantTests[F] { def laws: InvariantLaws[F] = InvariantLaws[F] }
}
