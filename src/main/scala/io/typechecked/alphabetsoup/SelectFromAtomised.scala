package io.typechecked.alphabetsoup

/**
 * A trait that extracts or replaces a value U (atom or molecule) from a type L, provided the type L
 * has been atomised first.
 *
 * If no such value exists, it does not compile
 *
 * ## Molecules
 *
 * ### Selecting
 *
 * You can select an M[B] from a structure as long as you can select an M[A], where a Mixer[B, A] exists.
 *
 * The M[A] is simply mapped over with the mixer. In the case where B = A, the same functionality occurs without
 * the mixer step
 *
 * ### Replacing
 *
 * If you replace a List[A] with a List[A], the entire structure will be replaced by the new list.
 * Ie, the length will be the length of the new list.
 *
 * This is DIFFERENT behaviour to the selection case, where the original structure is preserved. Molecules act as
 * atoms for replacing, as you would expect if you replaced a list with another list; you would expect the
 * length to be the length of the new list.
 *
 * You are unable to replace an M[B] with an M[A] if B != A, even if there is a Mixer[B, A] present. We simply ignore
 * the replacement request.
 *
 * This is equivalent to the bahviour you would expect of Mixer[(B, A), A], where a Miaxer[A, B] exists
 */
trait SelectFromAtomised[L, U]:

  /** Select the first instance of U found within L */
  def apply(l: L): U

  /** Replace the first instance of U found within l with u */
  def replace(u: U, l: L): L


// TODO: Atom/Molecule paths could be unified or split more sensibly
object SelectFromAtomised extends LowPrioritySelectFromAtomised:

  def apply[L, U](using as: SelectFromAtomised[L, U]): SelectFromAtomised[L, U] = as

  given headAtomSelect[H: Atom, T <: Tuple]: SelectFromAtomised[H *: T, H] =
    new SelectFromAtomised[H *: T, H]:
      def apply(l: H *: T): H = l.head
      def replace(u: H, l: H *: T): H *: T = u *: l.tail

  given preciseHeadSelectMolecule[M[_], A, T <: Tuple](using molecule: Molecule[M, A]): SelectFromAtomised[M[A] *: T, M[A]] =
    new SelectFromAtomised[M[A] *: T, M[A]]:
      def apply(t: M[A] *: T): M[A] = t.head
      def replace(u: M[A], l: M[A] *: T): M[A] *: T = u *: l.tail

  given recurse[H, T <: Tuple, U: Atom](using st: SelectFromAtomised[T, U]): SelectFromAtomised[H *: T, U] =
    new SelectFromAtomised[H *: T, U]:
      def apply(l: H *: T): U = st(l.tail)
      def replace(u: U, l: H *: T): H *: T = l.head *: st.replace(u, l.tail)

  given recurseNested[H <: Tuple, T <: Tuple, U: Atom](
    using st: => SelectFromAtomised[H, U]
  ): SelectFromAtomised[H *: T, U] =
    new SelectFromAtomised[H *: T, U]:
      def apply(l: H *: T): U = st(l.head)
      def replace(u: U, l: H *: T): H *: T = st.replace(u, l.head) *: l.tail


trait LowPrioritySelectFromAtomised extends LowLowPrioritySelectFromAtomised:

  // case where A & B are isomorphic
  given fuzzyHeadSelectMoleculeABIso[M[_], A, T <: Tuple, B](using
    molecule: Molecule[M, B],
    ev: A =:!= B,
    mixer: => Mixer[A, B],
    mixer2: => Mixer[B, A]
  ): SelectFromAtomised[M[A] *: T, M[B]] =
    new SelectFromAtomised[M[A] *: T, M[B]]:
      def apply(t: M[A] *: T): M[B] = molecule.functor.map(t.head)(mixer.mix)
      def replace(u: M[B], l: M[A] *: T): M[A] *: T = molecule.functor.map(u)(mixer2.mix) *: l.tail

  given recurseTailMolecule[H, T <: Tuple, M[_], U](using
    molecule: Molecule[M, U],
    st: SelectFromAtomised[T, M[U]]
  ): SelectFromAtomised[H *: T, M[U]] =
    new SelectFromAtomised[H *: T, M[U]]:
      def apply(l: H *: T): M[U] = st(l.tail)
      def replace(u: M[U], l: H *: T): H *: T = l.head *: st.replace(u, l.tail)

  given recurseHeadMolecule[H <: Tuple, T <: Tuple, U, M[_]](using
    molecule: Molecule[M, U],
    st: => SelectFromAtomised[H, M[U]]
  ): SelectFromAtomised[H *: T, M[U]] =
    new SelectFromAtomised[H *: T, M[U]]:
      def apply(l: H *: T): M[U] = st(l.head)
      def replace(u: M[U], l: H *: T): H *: T = st.replace(u, l.head) *: l.tail

trait LowLowPrioritySelectFromAtomised:
  given fuzzyHeadSelectMolecule[M[_], A, T <: Tuple, B](using
    molecule: Molecule[M, B],
    ev: A =:!= B,
    mixer: Mixer[A, B]
  ): SelectFromAtomised[M[A] *: T, M[B]] =
    new SelectFromAtomised[M[A] *: T, M[B]]:
      def apply(t: M[A] *: T): M[B] = molecule.functor.map(t.head)(mixer.mix)
      def replace(u: M[B], l: M[A] *: T): M[A] *: T = l

