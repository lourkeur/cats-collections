package cats.collections

import cats._
import cats.data.AndThen

private object PredicateHelpers {
  // use the static Eval instances to reduce memory footprint
  @inline def boolToNow(b: Boolean): Eval[Boolean] =
    if (b) Eval.True else Eval.False

  trait FromApplyF[A] extends Predicate[A] {
    override def applyF(a: A): Eval[Boolean] =
      throw new NotImplementedError("applyF implementation missing")
    val applyAndThen = AndThen(applyF(_)).andThen(_.value)
  }
}

/**
 * An intensional set, which is a set which instead of enumerating its
 * elements as a extensional set does, it is defined by a predicate
 * which is a test for membership.
 */
sealed abstract class Predicate[-A] extends scala.Function1[A, Boolean] { self =>
  import PredicateHelpers._

  def apply(a: A): Boolean = applyAndThen(a)

  val applyAndThen: AndThen[A, Boolean]

  private[collections] def applyF(a: A): Eval[Boolean] = boolToNow(applyAndThen(a))

  /**
   * returns a predicate which is the union of this predicate and another
   */
  def union[B <: A](other: Predicate[B]): Predicate[B] = new FromApplyF[B] {
    override def applyF(a: B) = Eval.defer(self.applyF(a)).flatMap(if (_) Eval.True else other.applyF(a))
  }

  /**
   * returns a predicate which is the union of this predicate and another
   */
  def |[B <: A](other: Predicate[B]): Predicate[B] = self union other

  /**
   * returns a predicate which is the intersection of this predicate and another
   */
  def intersection[B <: A](other: Predicate[B]): Predicate[B] = new FromApplyF[B] {
    override def applyF(a: B) = Eval.defer(self.applyF(a)).flatMap(if (_) other.applyF(a) else Eval.False)
  }

  /**
   * returns a predicate which is the intersection of this predicate and another
   */
  def &[B <: A](other: Predicate[B]): Predicate[B] = self intersection other

  /**
   * Returns true if the value satisfies the predicate.
   */
  def contains(a: A): Boolean = apply(a)

  /**
   * Returns the predicate which is the the difference of another predicate removed from this predicate
   */
  def diff[B <: A](remove: Predicate[B]): Predicate[B] = self intersection remove.negate

  /**
   * Returns the predicate which is the the difference of another predicate removed from this predicate
   */
  def -[B <: A](remove: Predicate[B]): Predicate[B] = self diff remove

  /**
   * Return the opposite predicate
   */
  def negate: Predicate[A] = new Predicate[A] {
    val applyAndThen = self.applyAndThen.andThen(!_)
    override def negate = self  // double negation is identity
  }

  /**
   * Return the opposite predicate
   */
  def unary_!(): Predicate[A] = negate


  /**
   * Alias for [[compose]]
   */
  def contramap[B](f: B => A): Predicate[B] = compose(f)

  override def compose[B](f: B => A): Predicate[B] = new Predicate[B] {
    val applyAndThen = self.applyAndThen compose f
  }
  override def andThen[B](f: Boolean => B): AndThen[A, B] = applyAndThen andThen f
}

object Predicate extends PredicateInstances {
  def apply[A](f: A => Boolean): Predicate[A] = new Predicate[A] {
    val applyAndThen = AndThen(f)
  }

  def empty: Predicate[Any] = apply(_ => false)
}

trait PredicateInstances {
  implicit def predicateContravariantMonoidal: ContravariantMonoidal[Predicate] = new ContravariantMonoidal[Predicate] {
    override def contramap[A, B](fb: Predicate[A])(f: B => A): Predicate[B] =
      fb.contramap(f)
    override def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
      fa.contramap[(A,B)](_._1) union fb.contramap(_._2)
    override def unit: Predicate[Unit] = Predicate.empty
  }

  implicit def predicateMonoid[A]: Monoid[Predicate[A]] = new Monoid[Predicate[A]] {
    override def empty: Predicate[A] = Predicate.empty
    override def combine(l: Predicate[A], r: Predicate[A]): Predicate[A] = l union r
  }

  implicit val predicateMonoidK: MonoidK[Predicate] = new MonoidK[Predicate] {
    override def empty[A]: Predicate[A] = Predicate.empty
    override def combineK[A](l: Predicate[A], r: Predicate[A]): Predicate[A] = l union r
  }
}
