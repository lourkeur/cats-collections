package dogs

import Predef._
import scala.{Boolean,Product,Serializable,Unit}

/** Represents a right-biased disjunction that is either an `A`, or a
  * `B`, or both an `A` and a `B`.
 *
 * An instance of `A [[Ior]] B` is one of:
 *  - `[[Ior.Left Left]][A]`
 *  - `[[Ior.Right Right]][B]`
 *  - `[[Ior.Both Both]][A, B]`
 *
 * `A [[Ior]] B` is similar to `A [[Xor]] B`, except that it can
 * represent the simultaneous presence of an `A` and a `B`. It is
 * right-biased like [[Xor]], so methods such as `map` and `flatMap`
 * operate on the `B` value. Some methods, like `flatMap`, while other
 * methods, like [[toXor]], ignore the `A` value in a [[Ior.Both
 * Both]].
 *
 * `A [[Ior]] B` is isomorphic to `(A [[Xor]] B) [[Xor]] (A, B)`, but
 * provides methods biased toward `B` values, regardless of whether
 * the `B` values appear in a [[Ior.Right Right]] or a [[Ior.Both
 * Both]].  The isomorphic [[Xor]] form can be accessed via the
 * [[unwrap]] method.
 */
sealed abstract class Ior[+A, +B] extends Product with Serializable {
  import Option._

  final def fold[C](fa: A => C, fb: B => C, fab: (A, B) => C): C = this match {
    case Ior.Left(a) => fa(a)
    case Ior.Right(b) => fb(b)
    case Ior.Both(a, b) => fab(a, b)
  }

  final def isLeft: Boolean = fold(_ => true, _ => false, (_, _) => false)
  final def isRight: Boolean = fold(_ => false, _ => true, (_, _) => false)
  final def isBoth: Boolean = fold(_ => false, _ => false, (_, _) => true)

  final def left[AA >: A]: Option[AA] = fold(a => Some(a), _ => None(), (a, _) => Some(a))
  final def right[BB >: B]: Option[BB] = fold(_ => None(), b => Some(b), (_, b) => Some(b))
  final def onlyLeft[AA >: A]: Option[AA] = fold(a => Some(a), _ => None(), (_, _) => None())
  final def onlyRight[BB >: B]: Option[BB] = fold(_ => None(), b => Some(b), (_, _) => None())
  final def onlyLeftOrRight[AA >: A, BB >: B]: Option[AA Xor BB] = fold(a => Some(Xor.left(a)), b => Some(Xor.right(b)), (_, _) => None())
  final def onlyBoth[AA >: A, BB >: B]: Option[(AA, BB)] = fold(_ => None(), _ => None(), (a, b) => Some((a, b)))
  final def pad[AA >: A, BB >: B]: (Option[AA], Option[BB]) = fold(a => (Some(a), None()), b => (None(), Some(b)), (a, b) => (Some(a), Some(b)))
  final def unwrap[AA >: A, BB >: B]: (AA Xor BB) Xor (AA, BB) = fold(a => Xor.left(Xor.left(a)), b => Xor.left(Xor.right(b)), (a, b) => Xor.right((a, b)))

  final def toXor: A Xor B = fold(Xor.left, Xor.right, (_, b) => Xor.right(b))
  final def toOption[BB >: B]: Option[BB] = right

/*  final def to[F[_], BB >: B](implicit monoidKF: MonoidK[F], applicativeF: Applicative[F]): F[BB] =
    fold(_ => monoidKF.empty, applicativeF.pure, (_, b) => applicativeF.pure(b))
 */

  final def swap: B Ior A = fold(Ior.right, Ior.left, (a, b) => Ior.both(b, a))

  final def exists(p: B => Boolean): Boolean = right exists p
  final def forall(p: B => Boolean): Boolean = right forall p
  final def getOrElse[BB >: B](bb: => BB): BB = right getOrElse bb
/*
  final def valueOr[BB >: B](f: A => BB)(implicit BB: Semigroup[BB]): BB =
    fold(f, identity, (a, b) => BB.combine(f(a), b))
 */
  final def bimap[C, D](fa: A => C, fb: B => D): C Ior D =
    fold(a => Ior.left(fa(a)), b => Ior.right(fb(b)), (a, b) => Ior.both(fa(a), fb(b)))

  final def map[D](f: B => D): A Ior D = bimap(identity, f)
  final def leftMap[C](f: A => C): C Ior B = bimap(f, identity)

/*
  final def flatMap[AA >: A, D](f: B => AA Ior D)(implicit AA: Semigroup[AA]): AA Ior D = this match {
    case l @ Ior.Left(_) => l
    case Ior.Right(b) => f(b)
    case Ior.Both(a1, b) => f(b) match {
      case Ior.Left(a2) => Ior.Left(AA.combine(a1, a2))
      case Ior.Right(b) => Ior.Both(a1, b)
      case Ior.Both(a2, d) => Ior.Both(AA.combine(a1, a2), d)
    }
  }
 */
  final def foreach(f: B => Unit): Unit = {
    bimap(_ => (), f)
    ()
  }
/*
  final def traverse[F[_], AA >: A, D](g: B => F[D])(implicit F: Applicative[F]): F[AA Ior D] = this match {
    case Ior.Left(a) => F.pure(Ior.left(a))
    case Ior.Right(b) => F.map(g(b))(Ior.right)
    case Ior.Both(a, b) => F.map(g(b))(d => Ior.both(a, d))
  }
 */

  final def foldLeft[C](c: C)(f: (C, B) => C): C =
    fold(_ => c, f(c, _), (_, b) => f(c, b))

  final def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
    fold(_ => lc, f(_, lc), (_, b) => f(b, lc))

/*
  final def merge[AA >: A](implicit ev: B <:< AA, AA: Semigroup[AA]): AA =
    fold(identity, ev.apply, (a, b) => AA.combine(a, b))
 */

/*
  // scalastyle:off cyclomatic.complexity
  final def append[AA >: A, BB >: B](that: AA Ior BB)(implicit AA: Semigroup[AA], BB: Semigroup[BB]): AA Ior BB = this match {
    case Ior.Left(a1) => that match {
      case Ior.Left(a2) => Ior.Left(AA.combine(a1, a2))
      case Ior.Right(b2) => Ior.Both(a1, b2)
      case Ior.Both(a2, b2) => Ior.Both(AA.combine(a1, a2), b2)
    }
    case Ior.Right(b1) => that match {
      case Ior.Left(a2) => Ior.Both(a2, b1)
      case Ior.Right(b2) => Ior.Right(BB.combine(b1, b2))
      case Ior.Both(a2, b2) => Ior.Both(a2, BB.combine(b1, b2))
    }
    case Ior.Both(a1, b1) => that match {
      case Ior.Left(a2) => Ior.Both(AA.combine(a1, a2), b1)
      case Ior.Right(b2) => Ior.Both(a1, BB.combine(b1, b2))
      case Ior.Both(a2, b2) => Ior.Both(AA.combine(a1, a2), BB.combine(b1, b2))
    }
  }
  // scalastyle:on cyclomatic.complexity
 */

/*  final def ===[AA >: A, BB >: B](that: AA Ior BB)(implicit AA: Eq[AA], BB: Eq[BB]): Boolean = fold(
    a => that.fold(a2 => AA.eqv(a, a2), b2 => false, (a2, b2) => false),
    b => that.fold(a2 => false, b2 => BB.eqv(b, b2), (a2, b2) => false),
    (a, b) => that.fold(a2 => false, b2 => false, (a2, b2) => AA.eqv(a, a2) && BB.eqv(b, b2))
  )
 */
/*
  final def show[AA >: A, BB >: B](implicit AA: Show[AA], BB: Show[BB]): String = fold(
    a => s"Ior.Left(${AA.show(a)})",
    b => s"Ior.Right(${BB.show(b)})",
    (a, b) => s"Ior.Both(${AA.show(a)}, ${BB.show(b)})"
  )
 */
}
 
object Ior extends IorFunctions {
  final case class Left[+A](a: A) extends (A Ior Nothing)
  final case class Right[+B](b: B) extends (Nothing Ior B)
  final case class Both[+A, +B](a: A, b: B) extends (A Ior B)
}

sealed trait IorFunctions {
  import Option._

  def left[A, B](a: A): A Ior B = Ior.Left(a)
  def right[A, B](b: B): A Ior B = Ior.Right(b)
  def both[A, B](a: A, b: B): A Ior B = Ior.Both(a, b)

  /**
   * Create an `Ior` from two Options if at least one of them is defined.
   *
   * @param oa an element (optional) for the left side of the `Ior`
   * @param ob an element (optional) for the right side of the `Ior`
   *
   * @return `None` if both `oa` and `ob` are `None`. Otherwise `Some` wrapping
   * an [[Ior.Left]], [[Ior.Right]], or [[Ior.Both]] if `oa`, `ob`, or both are
   * defined (respectively).
   */
  def fromOptions[A, B](oa: Option[A], ob: Option[B]): Option[A Ior B] =
    oa match {
      case Some(a) => ob match {
        case Some(b) => Some(Ior.Both(a, b))
        case None() => Some(Ior.Left(a))
      }
      case None() => ob match {
        case Some(b) => Some(Ior.Right(b))
        case None() => None()
      }
    }
}
