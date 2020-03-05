package cats.collections

import cats._
import cats.data.AndThen

/**
 * An intensional set, which is a set which instead of enumerating its
 * elements as a extensional set does, it is defined by a predicate
 * which is a test for membership.
 */
sealed abstract class Predicate[-A] { self =>

  def apply(a: A): Boolean = applyAndThen(a)

  protected val applyAndThen: AndThen[A, Boolean]

  protected def applyEval(a: A): Eval[Boolean]

  /**
   * returns a predicate which is the union of this predicate and another
   */
  def union[B <: A](other: Predicate[B]): Predicate[B] = Predicate.fromEval[B] {
    a => Eval.defer(self.applyEval(a)).flatMap(if (_) Eval.True else other.applyEval(a))
  }

  /**
   * returns a predicate which is the union of this predicate and another
   */
  def |[B <: A](other: Predicate[B]): Predicate[B] = self union other

  /**
   * returns a predicate which is the intersection of this predicate and another
   */
  def intersection[B <: A](other: Predicate[B]): Predicate[B] = Predicate.fromEval[B] {
    a => Eval.defer(self.applyEval(a)).flatMap(if (_) other.applyEval(a) else Eval.False)
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
  def negate: Predicate[A] = Predicate[A](applyAndThen.andThen(!_))

  /**
   * Return the opposite predicate
   */
  def unary_!(): Predicate[A] = negate


  /**
   * Alias for [[compose]]
   */
  def contramap[B](f: B => A): Predicate[B] = compose(f)

  def compose[B](f: B => A): Predicate[B] = Predicate[B](applyAndThen compose f)
  def andThen[B](f: Boolean => B): AndThen[A, B] = applyAndThen andThen f
}

object Predicate extends PredicateInstances {
  // Note, we only have two subclasses in order to be gentle on the JVM JIT
  // which treats classes with more than two subclasses differently

  class FromEval[A](f: A => Eval[Boolean]) extends Predicate[A] {
    def applyEval(a: A) = f(a)
    val applyAndThen = AndThen(f).andThen(_.value)
  }
  class FromAndThen[A](val applyAndThen: AndThen[A, Boolean]) extends Predicate[A] {
    def applyEval(a: A) = applyAndThen(a) match {
      // use static instances to reduce memory pressure
      case true => Eval.True
      case false => Eval.False
    }
  }

  def apply[A](f: A => Boolean): Predicate[A] = new FromAndThen(AndThen(f))

  def fromEval[A](f: A => Eval[Boolean]): Predicate[A] = new FromEval(f)

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
