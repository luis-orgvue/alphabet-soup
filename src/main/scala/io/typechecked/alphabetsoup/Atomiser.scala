package io.typechecked.alphabetsoup

import shapeless3.deriving.K1.Generic

import scala.deriving.Mirror

// T => HList or Atom or Molecule
trait Atomiser[T] extends Serializable:
  type Repr
  def to(t : T) : Repr
  def from(r : Repr) : T

object Atomiser extends LowPriorityAtomiserImplicits:

  type Aux[T, Repr0] = Atomiser[T] { type Repr = Repr0 }

  def apply[T](using dg: Atomiser[T]): Atomiser.Aux[T, dg.Repr] = dg

  // Recurse on the head, and then on the tail
  given headFirstSearchCase[H, T <: Tuple, HOut, TOut <: Tuple](using
    atomiserH: => Atomiser.Aux[H, HOut],
    atomiserT: Atomiser.Aux[T, TOut],
  ): Atomiser.Aux[H *: T, HOut *: TOut] = new Atomiser[H *: T]:
    type Repr = HOut *: TOut
    def to(t: H *: T): HOut *: TOut = atomiserH.to(t.head) *: atomiserT.to(t.tail)
    def from(r: HOut *: TOut): H *: T = atomiserH.from(r.head) *: atomiserT.from(r.tail)

  // If we've hit an Atom we go no deeper
  given atomCase[T](using ev: Atom[T]): Atomiser.Aux[T, T] =
    new Atomiser[T]:
      type Repr = T
      def to(t: T): T = t
      def from(u: T): T = u

  // If we've hit a molecule we go no deeper
  given moleculeCase[M[_], T](
    using ev: Molecule[M, T]
  ): Atomiser.Aux[M[T], M[T]] =
    new Atomiser[M[T]]:
      type Repr = M[T]
      def to(t: M[T]): M[T] = t
      def from(u: M[T]): M[T] = u


trait LowPriorityAtomiserImplicits:
  // Turn T into an HList and recurse. Low priority because we should match on T being HList first
  given genericCase[T <: Product, AtomiserOut <: Tuple](using
    gen: Mirror.ProductOf[T],
    atomiser: => Atomiser.Aux[gen.MirroredElemTypes, AtomiserOut]
  ): Atomiser.Aux[T, AtomiserOut] = new Atomiser[T]:
    type Repr = AtomiserOut
    def to(t: T): AtomiserOut = atomiser.to(Tuple.fromProductTyped(t))
    def from(r: AtomiserOut): T = gen.fromProduct(atomiser.from(r))
