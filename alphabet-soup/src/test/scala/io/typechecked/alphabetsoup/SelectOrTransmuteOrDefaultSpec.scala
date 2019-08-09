package io.typechecked
package alphabetsoup

import org.scalatest._
import shapeless.{::, HNil}
import shapeless.test.illTyped

import Atom.DefaultAtom

// TODO: nested structure tests
class SelectOrTransmuteOrDefaultSpec extends FlatSpec with Matchers {

  "SelectOrDefault" should "select a value from an atomised structure even with a default atom present" in {
    type L = Int :: String :: HNil
    type U = String

    val l = 17 :: "twine" :: HNil

    implicit val defaultU: DefaultAtom[U] = DefaultAtom[U]("default")

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe "twine"
  }

  it should "replace a value in an atomised structure even with a default atom present" in {
    type L = Int :: String :: HNil
    type U = String

    val l = 17 :: "twine" :: HNil

    implicit val defaultU: DefaultAtom[U] = DefaultAtom[U]("default")

    SelectOrTransmuteOrDefault[L, U].replace("yarn", l) shouldBe 17 :: "yarn" :: HNil
  }

  it should "select a default from an atomised structure, if there is a default present but the type is not in the structure" in {
    type L = Int :: String :: HNil
    type U = Boolean

    val l = 17 :: "twine" :: HNil

    implicit val defaultU: DefaultAtom[U] = DefaultAtom[U](false)

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe false
  }

  it should "replace nothing if there is a default present for a type not in the original structure" in {
    type L = Int :: String :: HNil
    type U = Boolean

    val l = 17 :: "twine" :: HNil

    implicit val defaultU: DefaultAtom[U] = DefaultAtom[U](false)

    SelectOrTransmuteOrDefault[L, U].replace(true, l) shouldBe l
  }

  it should "not compile if the type we want is not present and there is no default atom" in {
    type L = Int :: String :: HNil
    type U = Boolean

    illTyped("SelectOrTransmuteOrDefault[L, U]", ".*could not find implicit value.*SelectOrTransmuteOrDefault.*")
  }
  it should "for type not present select and map an atomic value if there is a transmutation in scope" in {
    implicit val transmute = Transmute[String, Boolean](_ => true)
    type L = Int :: String :: HNil
    type U = Boolean

    val l = 17 :: "twine" :: HNil

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe true
  }

  it should "for type not present select and map molecule if there is a transmutation in scope" in {
    implicit val transmute = Transmute.molecular[List, String, String]{ ls: List[String] => ls.mkString(" ")}
    type L = Int :: List[String] :: HNil
    type U = String

    val l = 17 :: List("hello", "everybody") :: HNil

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe "hello everybody"
  }

  it should "for type not present select and map molecule at head functorally if there is a transmutation in scope" in {
    implicit val transmute = Transmute.transmuteF[List, String, Int](_.length)
    type L = List[String] :: HNil
    type U = List[Int]

    val l = List("hi", "there") :: HNil

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe 2 :: 5 :: Nil
  }

  it should "for type not present select and map in a nested list" in {
    implicit val transmute = Transmute[String, Boolean](_ => true)
    type L = Int :: (Int :: String :: HNil) :: HNil
    type U = Boolean

    val l = 17 :: (14 :: "twine" :: HNil) :: HNil

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe true
  }

  it should "for type not present select and map molecule functorally if there is a transmutation in scope" in {
    implicit val transmute = Transmute.transmuteF[List, String, Int](_.length)
    type L = Int :: List[String] :: HNil
    type U = List[Int]

    val l = 17 :: List("hi", "there") :: HNil

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe 2 :: 5 :: Nil
  }

  it should "not compile when type not in list and no transmutation or default in scope" in {
    type L = Int :: String :: HNil
    type U = Boolean

    val l = 17 :: "twine" :: HNil

    illTyped("SelectOrTransmuteOrDefault[L, U].apply(l)", ".*could not find implicit value.*SelectOrTransmuteOrDefault.*")
  }

  it should "for type not present use first transmutation possible if two are available" in {
    implicit val transmuteString = Transmute[String, Boolean](_ => true)
    implicit val transmuteInt = Transmute[Int, Boolean](_ => false)

    type L = Int :: String :: HNil
    type U = Boolean

    val l = 17 :: "twine" :: HNil

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe false
  }

  it should "for type not present transmute the first available value if multiple in list" in {
    implicit val transmute = Transmute[String, Int](_.length)
    type L = String :: String :: HNil
    type U = Int

    val l = "hi" :: "twine" :: HNil

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe 2
  }

  it should "try to perform transmutations before inserting defaults" in {
    implicit val defaultU: DefaultAtom[U] = DefaultAtom[U](false)
    implicit val bob: Transmute[String, Boolean] = Transmute.apply[String, Boolean](_ => true)
    type L = Int :: String :: HNil
    type U = Boolean

    val l = 17 :: "twine" :: HNil

    SelectOrTransmuteOrDefault[L, U].apply(l) shouldBe true
  }

}
