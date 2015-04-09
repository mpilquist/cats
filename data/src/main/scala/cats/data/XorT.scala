package cats
package data

case class XorT[F[_], A, B](value: F[A Xor B]) {

  def fold[C](fa: A => C, fb: B => C)(implicit F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb))

  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  def swap(implicit F: Functor[F]): XorT[F, B, A] = XorT(F.map(value)(_.swap))

  //def foreach(f: B => Unit)(implicit F: : F[Unit] = F.map(value)(_.foreach(f))
  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

  def toEither(implicit F: Functor[F]): F[Either[A, B]] = F.map(value)(_.toEither)

  def toOption(implicit F: Functor[F]): F[Option[B]] = F.map(value)(_.toOption)

  def to[G[_]](implicit functorF: Functor[F], monoidKG: MonoidK[G], applicativeG: Applicative[G]): F[G[B]] =
    functorF.map(value)(_.to[G, B])

  def todo(implicit F: MonadCombine[F]): F[B] =
    F.flatMap(value)(_.to[F, B])

  def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): XorT[F, C, D] = XorT(F.map(value)(_.bimap(fa, fb)))

  def flatMap[AA >: A, D](f: B => XorT[F, AA, D])(implicit F: Monad[F]): XorT[F, AA, D] =
    XorT(F.flatMap(value) {
      case l @ Xor.Left(_) => F.pure(l)
      case Xor.Right(b) => f(b).value
    })

  def flatMapF[AA >: A, D](f: B => F[AA Xor D])(implicit F: Monad[F]): XorT[F, AA, D] =
    flatMap(f andThen XorT.apply)

  def map[D](f: B => D)(implicit F: Functor[F]): XorT[F, A, D] = bimap(identity, f)

  def leftMap[C](f: A => C)(implicit F: Functor[F]): XorT[F, C, B] = bimap(f, identity)

  def compare(that: XorT[F, A, B])(implicit o: Order[F[A Xor B]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: XorT[F, A, B])(implicit p: PartialOrder[F[A Xor B]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: XorT[F, A, B])(implicit eq: Eq[F[A Xor B]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], D](f: B => G[D])(implicit traverseF: Traverse[F], traverseXorA: Traverse[A Xor ?], applicativeG: Applicative[G]): G[XorT[F, A, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => traverseXorA.traverse(axb)(f)))(XorT.apply)

  def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c)((c, axb) => axb.fold(_ => c, f(c, _)))

  def foldRight[C](c: C)(f: (B, C) => C)(implicit F: Foldable[F]): C =
    F.foldRight(value, c)((axb, c) => axb.fold(_ => c, f(_, c)))

  def partialFold[C](f: B => Fold[C])(implicit F: Foldable[F]): Fold[C] =
    F.partialFold(value)(axb => axb.fold(_ => Fold.Pass, f))

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F]): F[AA] = F.map(value)(_.fold(identity, ev.apply))

  def show(implicit show: Show[F[A Xor B]]): String = show.show(value)
}
