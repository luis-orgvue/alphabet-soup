package io.typechecked.alphabetsoup

import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// TODO: nested structure tests
class SelectFromAtomisedSpec extends AnyFlatSpec with Matchers {

  "SelectFromAtomised" should "select an atomic value from an atomised structure" in {
    type L = Int *: String *: EmptyTuple
    type U = String

    val l = 17 *: "twine" *: EmptyTuple

    SelectFromAtomised[L, U].apply(l) shouldBe "twine"
  }

  it should "select a molecule from an atomised structure" in {
    type L = Int *: List[String] *: EmptyTuple
    type U = List[String]

    val l = 17 *: List("twine", "yarn") *: EmptyTuple

    SelectFromAtomised[L, U].apply(l) shouldBe List("twine", "yarn")
  }

  it should "fuzzily select a molecule from an atomised structure" in {
    case class A(str: String)
    case class B(str: String)

    type L = Int *: List[A] *: EmptyTuple
    type U = List[B]

    val l = 17 *: List(A("twine"), A("yarn")) *: EmptyTuple

    SelectFromAtomised[L, U].apply(l) shouldBe List(B("twine"), B("yarn"))
  }

  it should "replace an atomic value from an atomised structure" in {
    type L = Int *: String *: EmptyTuple
    type U = String

    val l = 17 *: "twine" *: EmptyTuple

    SelectFromAtomised[L, U].replace("thread", l) shouldBe 17 *: "thread" *: EmptyTuple
  }

  it should "replace a molecule from an atomised structure" in {
    type L = Int *: List[String] *: EmptyTuple
    type U = List[String]

    val l = 17 *: List("twine", "yarn") *: EmptyTuple

    SelectFromAtomised[L, U].replace(List("thread"), l) shouldBe 17 *: List("thread") *: EmptyTuple
  }

  it should "NOT fuzzily replace a molecule from an atomised structure if the internal types cannot be mixed to one another" in {

    // Fuzziness does NOT work with replacing molecules

    // A and B are not isomorphic; we have Mixer[A, B] but not Mixer[B, A]
    case class A(str: String, int: Int)
    case class B(str: String)

    type L = Int *: List[A] *: EmptyTuple
    type U = List[B]

    val l = 17 *: List(A("twine", 0), A("yarn", 2)) *: EmptyTuple

    SelectFromAtomised[L, U].replace(List(B("thread")), l) shouldBe l
  }

  it should "fuzzily replace a molecule from an atomised structure if the internal types are isomorphic" in {

    // Fuzziness DOES work with replacing molecules if the internal types are isomorphic

    case class A(str: String)
    case class B(str: String)

    type L = Int *: List[A] *: EmptyTuple
    type U = List[B]

    val l = 17 *: List(A("twine"), A("yarn")) *: EmptyTuple

    SelectFromAtomised[L, U].replace(List(B("thread")), l) shouldBe 17 *: List(A("thread")) *: EmptyTuple
  }

  it should "not compile for an atomic value from a non-atomised structure" in {
    case class L(int: Int, str: String)
    type U = String

    illTyped("SelectFromAtomised[L, U]", ".*No given instance of type io.typechecked.alphabetsoup.SelectFromAtomised.*")
  }

  it should "not compile for a molecule from a non-atomised structure" in {
    case class L(int: Int, str: List[String])
    type U = List[String]

    illTyped("SelectFromAtomised[L, U]", ".*No given instance of type io.typechecked.alphabetsoup.SelectFromAtomised.*")
  }

  it should "not compile for fuzzily selecting a molecule from a non-atomised structure" in {

    case class A(str: String)
    case class B(str: String)

    case class L(int: Int, str: List[A])
    type U = List[B]

    illTyped("SelectFromAtomised[L, U]", ".*No given instance of type io.typechecked.alphabetsoup.SelectFromAtomised.*")
  }

  it should "not compile if the type is not present" in {
    type L = Int *: String *: EmptyTuple
    type U = Boolean

    illTyped("SelectFromAtomised[L, U]", ".*No given instance of type io.typechecked.alphabetsoup.SelectFromAtomised.*")
  }

  it should "not compile if the type is not present even if there is a default present" in {
    type L = Int *: String *: EmptyTuple
    type U = Boolean

    implicit val default: Atom.DefaultAtom[U] = Atom.DefaultAtom[U](false)

    illTyped("SelectFromAtomised[L, U]", ".*No given instance of type io.typechecked.alphabetsoup.SelectFromAtomised.*")
  }
}
