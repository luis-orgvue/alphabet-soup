package io.typechecked

import scala.annotation.implicitNotFound

package object alphabetsoup {
  def unexpected : Nothing = sys.error("Unexpected invocation")

  // Type inequalities
  trait =:!=[A, B] extends Serializable

  given neq[A, B]: =:!=[A, B] = new =:!=[A, B] {}

  given neqAmbig1[A]: =:!=[A, A] = unexpected

  given neqAmbig2[A]: =:!=[A, A] = unexpected
}
