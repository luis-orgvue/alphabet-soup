package io.typechecked.alphabetsoup

/**
 * Tries to use SelectFromAtomised to produce a value U from L. If it fails, tries to find a default atom
 * of type U. If this fails, it tries to transmute the value if there exists Transmute[T, U] from some type
 * T in L.
 *
 * This assumes L is atomised.
 *
 * ### Replace behaviour
 *
 * If we can select U from L, replace acts as it does for SelectFromAtomised
 *
 * If we can not select U from L and rely on an implicit default, we replace nothing and return our L unchanged
 */
trait SelectOrDefaultOrTransmute[L, U]:
  def apply(l: L): U
  def replace(u: U, l: L): L

object SelectOrDefaultOrTransmute extends LowPrioritySelectOrDefaultOrTransmute:

  def apply[L, U](using s: SelectOrDefaultOrTransmute[L, U]): SelectOrDefaultOrTransmute[L, U] = s

  given trySelectFromAtomised[L, U](using selector: SelectFromAtomised[L, U]): SelectOrDefaultOrTransmute[L, U] =
    new SelectOrDefaultOrTransmute[L, U]:
      def apply(l: L): U = selector(l)
      def replace(u: U, l: L): L = selector.replace(u, l)


trait LowPrioritySelectOrDefaultOrTransmute extends LowLowPrioritySelectOrDefaultOrTransmute:

  given tryDefaultSelect[L, U](using defaultAS: Atom.DefaultAtom[U]): SelectOrDefaultOrTransmute[L, U] =
    new SelectOrDefaultOrTransmute[L, U]:
      def apply(l: L): U = defaultAS.default
      def replace(u: U, l: L): L = l

trait LowLowPrioritySelectOrDefaultOrTransmute:
  given tryTransmute[L, U, T](using transmute: SelectTransmuted[L, U]): SelectOrDefaultOrTransmute[L, U] =
    new SelectOrDefaultOrTransmute[L, U]:
      def apply(l: L): U = transmute(l)
      def replace(u: U, l: L): L = l
