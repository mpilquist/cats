package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._

class ApplyTestImplicits[F[_], A, B, C](implicit
  val arbFAtoB: Arbitrary[F[A => B]],
  val arbFBtoC: Arbitrary[F[B => C]],
  val invariant: InvariantTestImplicits[F, A, B, C]
)

object ApplyTestImplicits {
  implicit def materialize[F[_], A, B, C](implicit
    arbFAtoB: Arbitrary[F[A => B]],
    arbFBtoC: Arbitrary[F[B => C]],
    invariant: InvariantTestImplicits[F, A, B, C]
  ): ApplyTestImplicits[F, A, B, C] =
    new ApplyTestImplicits()(arbFAtoB, arbFBtoC, invariant)
}
trait ApplyTests[F[_]] extends FunctorTests[F] {
  def laws: ApplyLaws[F]

  def apply[A, B, C](implicit implicits: ApplyTestImplicits[F, A, B, C]): RuleSet = {
    import implicits._
    import implicits.invariant._
    new DefaultRuleSet(
      name = "apply",
      parent = Some(functor[A, B, C]),
      "apply composition" -> forAll(laws.applyComposition[A, B, C] _))
  }
}

object ApplyTests {
  def apply[F[_]: Apply]: ApplyTests[F] =
    new ApplyTests[F] { def laws: ApplyLaws[F] = ApplyLaws[F] }
}
