package io.typechecked.alphabetsoup

import scala.quoted.{Expr, Quotes, Type}

trait Atom[T]

object Atom {

  def apply[T]: Atom[T] = new Atom[T] {}

  given stringAtom: Atom[String] = new Atom[String] {}
  given charAtom: Atom[Char] = new Atom[Char] {}
  given booleanAtom: Atom[Boolean] = new Atom[Boolean] {}
  given intAtom: Atom[Int] = new Atom[Int] {}
  given floatAtom: Atom[Float] = new Atom[Float] {}
  given doubleAtom: Atom[Double] = new Atom[Double] {}
  given longAtom: Atom[Long] = new Atom[Long] {}

  sealed trait DefaultAtom[T] extends Atom[T] {
    def default: T
  }

  object DefaultAtom {
    def apply[T](arg: T): DefaultAtom[T] = new DefaultAtom[T] {
      def default: T = arg
    }
  }

  given emptyTupleAtom: DefaultAtom[EmptyTuple] = DefaultAtom.apply[EmptyTuple](EmptyTuple)
  given unitAtom: DefaultAtom[Unit] = DefaultAtom.apply[Unit](())


  inline def derived[T]: Atom[T] = ${ derivedMacro[T] }

  def derivedMacro[T: Type](using Quotes): Expr[Atom[T]] =
    '{ new Atom[T]{} }

}
