package io.typechecked.alphabetsoup

// TODO rename, now also handles molecules
trait AtomSelector[L, U] {
  def apply(l: L): U
  def replace(u: U, l: L): L
}

object AtomSelector {

  def apply[L, U](using atomSelector: AtomSelector[L, U]): AtomSelector[L, U] = atomSelector

  given atomiseThenDelegate[L, LOut, U](using
    atomiser: Atomiser.Aux[L, LOut],
    s: SelectOrDefaultOrTransmute[LOut, U]
  ): AtomSelector[L, U] = new AtomSelector[L, U] {
    def apply(t: L): U = s(atomiser.to(t))
    def replace(u: U, l: L): L = atomiser.from(s.replace(u, atomiser.to(l)))
  }

}
