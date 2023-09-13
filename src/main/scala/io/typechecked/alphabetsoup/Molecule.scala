package io.typechecked.alphabetsoup

import cats.Functor
import cats.instances.list.catsStdInstancesForList
import cats.instances.option.catsStdInstancesForOption

trait Molecule[M[_], A] {
  def functor: Functor[M]
}

object Molecule {

  def apply[M[_], A](using f: Functor[M]): Molecule[M, A] = new Molecule[M, A] { val functor = f }

  given listMolecule[A]: Molecule[List, A] = Molecule[List, A]
  given optionMolecule[A]: Molecule[Option, A] = Molecule[Option, A]
}
