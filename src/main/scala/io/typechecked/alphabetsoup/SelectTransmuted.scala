package io.typechecked.alphabetsoup

trait SelectTransmuted[L, U]:
  def apply(l: L): U

object SelectTransmuted extends LowPrioritySelectAndTransmute:

  given tryHeadAtom[L <: Tuple, T: Atom, U](using transmute: Transmute[T, U]): SelectTransmuted[T *: L, U] =
    (l: T *: L) => transmute.convert(l.head)

  given tryHeadMolecule[L <: Tuple, M[_], T, U](using
    molecule: Molecule[M, T],
    transmute: Transmute[M[T], U]
  ): SelectTransmuted[M[T] *: L, U] =
    (l: M[T] *: L) => transmute.convert(l.head)

  given recurseNested[L <: Tuple, T <: Tuple, U](using transmutation: SelectTransmuted[T, U]): SelectTransmuted[T *: L, U] =
    (l: T *: L) => transmutation(l.head)

trait LowPrioritySelectAndTransmute:
  given recurseTail[L <: Tuple, T, U](using transmutation: SelectTransmuted[L, U]): SelectTransmuted[T *: L, U] =
    (l: T *: L) => transmutation(l.tail)

