package io.typechecked.alphabetsoup

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AtomMacroSpec extends AnyFlatSpec with Matchers {

  "@Atomic" should "create implicit Atom for an empty case class" in {
    case class Foo() derives Atom

    implicitly[Atom[Foo]]
  }

  it should "create an implicit Atom for a case class with a single field" in {
    case class Foo(bar: String) derives Atom

    implicitly[Atom[Foo]]
  }

  it should "create an implicit Atom for a case class with a multiple fields" in {
    case class Foo(bar: String, baz: Int, bam: Float) derives Atom

    implicitly[Atom[Foo]]
  }

  it should "create an implicit Atom for a case class with a multiple fields and function defintions inside" in {
    case class Foo(bar: String, baz: Int, bam: Boolean) derives Atom {
      def getBar: String = bar
      def getBaz: Int = baz
    }

    implicitly[Atom[Foo]]

    val foo = Foo("bar", 7, true)

    foo.getBar shouldBe "bar"
    foo.getBaz shouldBe 7
    foo.bam shouldBe true
  }

  it should "place implicit inside companion object if one already exists" in {
    case class Foo() derives Atom

    object Foo {
      val iAmHere: String = "Yeah Baby!"
    }

    implicitly[Atom[Foo]]

    Foo.iAmHere shouldBe "Yeah Baby!"

  }

  it should "place implicit inside companion object if one already exists for a case class with multiple fields" in {
    case class Foo(bar: String, baz: Int, bam: Boolean) derives Atom {

      def giveMeBam: Boolean = bam
    }

    object Foo {
      val iAmHere: String = "Yeah Baby!"
    }

    implicitly[Atom[Foo]]

    val foo = Foo("plumbus", 42, false)

    Foo.iAmHere shouldBe "Yeah Baby!"
    foo.bar shouldBe "plumbus"
    foo.baz shouldBe 42
    foo.giveMeBam shouldBe false
  }

  it should "blow up with ambiguous implicits if an Atom is already defined" in {
    case class Foo() derives Atom

    object Foo {
      implicit val atom: Atom[Foo] = Atom[Foo]
    }

    illTyped("implicitly[Atom[Foo]]",".*No given instance of type io.typechecked.alphabetsoup.Atom.*")
  }

  it should "create an implicit Atom for a Class" in {
    class Foo() derives Atom

    implicitly[Atom[Foo]]
  }

  it should "create an implicit Atom for a protected sealed abstract protected Class" in {

    implicitly[Atom[ProtectedFoo]]
  }

  it should "create an implicit Atom for a Trait" in {
    trait Foo derives Atom

    implicitly[Atom[Foo]]
  }

  it should "create an implicit Atom for a Trait with existing functions" in {
    trait Foo derives Atom {
      def bar: String = "bar"
    }

    implicitly[Atom[Foo]]

    val foo = new Foo {}
    foo.bar shouldBe "bar"
  }

  it should "create an implicit Atom for nested classes" in {
    class A {
      class B derives Atom
      object B
      implicitly[Atom[B]]
    }
  }

  it should "create an implicit Atom when companion object is far away" in {
    class A derives Atom

    trait B
    trait C derives Atom

    object A

    implicitly[Atom[A]]
    implicitly[Atom[C]]

  }

  it should "create an implicit Atom for a class defined inside a function" in {
    def foo = {
      class A derives Atom

      implicitly[Atom[A]]
    }
  }

  it should "create an implicit Atom for a class with a private constructor" in {
    class Foo private(bar:Int, val baz:String) derives Atom

    implicitly[Atom[Foo]]
  }

  it should "create an implicit Atom for private package classes" in {
    implicitly[Atom[PrivateFoo]]
  }

  it should "create an implicit Atom for a class with a type parameter" in {
    class Foo[A] derives Atom
    implicitly[Atom[Foo[String]]]
  }

  /*it should "create an implicit Atom for a class with a higher-kinded type parameter" in {
    class Foo[A[_]] derives Atom
    implicitly[Atom[Foo[Option]]]
  }

  it should "create an implicit Atom for a class with many type parameters" in {
    class Foo[A[_], B, C] derives Atom
    implicitly[Atom[Foo[Option, String, (Boolean, List[String])]]]
  }

  it should "create an implicit Atom for a class with a type parameter with a type bound" in {
    trait S
    class T extends S
    class U extends T
    class Foo[A <: T] derives Atom
    implicitly[Atom[Foo[U]]]
    illTyped(
      "implicitly[Atom[Foo[S]]]",
      "could not find implicit value for parameter.*"
    )

    class Bar[B >: T] derives Atom
    implicitly[Atom[Bar[S]]]
    illTyped(
      "implicitly[Atom[Bar[U]]]",
      "could not find implicit value for parameter.*"
    )

    class Faz[A >: U <: S] derives Atom
    implicitly[Atom[Faz[T]]]
    illTyped(
      "implicitly[Atom[Faz[String]]]",
      "could not find implicit value for parameter.*"
    )
  }*/

  it should "create an implicit Atom for a class whose companion object is implicit" in {
    class T derives Atom
    implicit object T {
      def bob: String = "bob"
    }
    implicitly[Atom[T]]
  }

  it should "create an implicit Atom for a class whose companion object has multiple modifiers" in {
    class T derives Atom
    implicit final object T {
      def bob: String = "bob"
    }
    implicitly[Atom[T]]
  }

}

protected sealed abstract class ProtectedFoo(implicit impl: DummyImplicit) derives Atom

private[alphabetsoup] class PrivateFoo derives Atom
