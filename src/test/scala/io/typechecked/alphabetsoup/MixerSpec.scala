package io.typechecked.alphabetsoup

import cats.Functor
import Atom.DefaultAtom
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

case class Age(i: Int) extends AnyVal
case class FirstName(value: String) extends AnyVal
case class LastName(value: String) extends AnyVal
case class Address1(value: String) extends AnyVal
case class Address2(value: String) extends AnyVal
case class City(value: String) extends AnyVal
case class Postcode(value: String) extends AnyVal

class MixerSpec extends AnyFlatSpec with Matchers:

  case class Single(s: String)

  case class A1(i: Int, s: String)

  case class A2(s2: String, i2: Int)

  case class B(b: Boolean, s: String)

  case class N1(a1: A1, a2: A2)

  case class N2(a1: A1, b: B)

  "Mixer" should "work on simple type equality" in:
    Mixer[String, String].mix("hello") shouldBe "hello"
    Mixer[String *: EmptyTuple, String *: EmptyTuple].mix("hello" *: EmptyTuple) shouldBe "hello" *: EmptyTuple
    Mixer[Tuple1[String], Tuple1[String]].mix(Tuple1("hello")) shouldBe Tuple1("hello")
    Mixer[A1, A1].mix(A1(5, "hello")) shouldBe A1(5, "hello")

  it should "select smaller type from larger for primitives" in:
    implicitly[Mixer[(String, Int), Int]]
    implicitly[Mixer[(String, Int), String]]

  it should "select smaller type from larger for non-primitives" in:

    class A
    given aAtom: Atom[A] = new Atom[A] {}

    class B
    given bAtom: Atom[B] = new Atom[B] {}

    "implicitly[Mixer[(A, B), B]]" should compile

    trait C
    given cAtom: Atom[C] = new Atom[C] {}
    trait D
    given dAtom: Atom[D] = new Atom[D] {}

    implicitly[Mixer[(C, D), D]]
    implicitly[Mixer[(C, D), C]]

    case class E()
    given eAtom: Atom[E] = new Atom[E] {}
    case class F()
    given fAtom: Atom[F] = new Atom[F] {}

    "implicitly[Mixer[(E, F), F]]" should compile


  it should "not work on mis-matched simple types" in:
    illTyped("""Mixer[Int, String]""")

  it should "read simple atoms from the LHS" in:
    Mixer[(Int, String), Int].mix(5 -> "hello") shouldBe 5

  it should "read simple molecules from the LHS" in:
    Mixer[(Int, List[String]), List[String]].mix(5 -> List("hello", "two", "three")) shouldBe List("hello", "two", "three")

  it should "read fuzzy molecules from the LHS" in:
    val m = Mixer[(Int, List[(String, Boolean)]), List[String]]
    m.mix(5 -> List("hello" -> true, "two" -> false)) shouldBe List("hello", "two")

  it should "work on simple one-length transformations" in:
    val string = "hello"

    Mixer[Tuple1[String], String *: EmptyTuple].mix(Tuple1(string)) shouldBe string *: EmptyTuple
    Mixer[Tuple1[String], Single].mix(Tuple1(string)) shouldBe Single(string)

    Mixer[String *: EmptyTuple, Tuple1[String]].mix(string *: EmptyTuple) shouldBe Tuple1(string)
    Mixer[String *: EmptyTuple, Single].mix(string *: EmptyTuple) shouldBe Single(string)

    Mixer[Single, String *: EmptyTuple].mix(Single(string)) shouldBe string *: EmptyTuple
    Mixer[Single, Tuple1[String]].mix(Single(string)) shouldBe Tuple1(string)

  it should "not work on simple one-length transformations if the atomic types do not match" in:
    illTyped("Mixer[Int *: EmptyTuple, Tuple1[String]]")
    illTyped("Mixer[Single, Tuple1[Int]]")

  it should "work on longer collections, including swapping type order" in:
    Mixer[Int *: String *: EmptyTuple, Int *: String *: EmptyTuple].mix(5 *: "s" *: EmptyTuple) shouldBe 5 *: "s" *: EmptyTuple
    Mixer[Int *: String *: EmptyTuple, String *: Int *: EmptyTuple].mix(5 *: "s" *: EmptyTuple) shouldBe "s" *: 5 *: EmptyTuple
    Mixer[Int *: String *: EmptyTuple, (String, Int)].mix(5 *: "s" *: EmptyTuple) shouldBe ("s" -> 5)
    Mixer[Int *: String *: EmptyTuple, A1].mix(5 *: "s" *: EmptyTuple) shouldBe A1(5, "s")
    Mixer[Int *: String *: EmptyTuple, A2].mix(5 *: "s" *: EmptyTuple) shouldBe A2("s", 5)
    Mixer[(String, Int), A1].mix("s" -> 5) shouldBe A1(5, "s")
    Mixer[A2, (Int, String)].mix(A2("s", 5)) shouldBe (5 -> "s")
    Mixer[A1, A2].mix(A1(5, "s")) shouldBe A2("s", 5)
    Mixer[A2, A1].mix(A2("s", 5)) shouldBe A1(5, "s")

  it should "not work if there is an atom in the target not present in the source" in:
    // String not in LHS
    illTyped("Mixer[Int *: EmptyTuple, (String, Int)]")

  it should "use a transmutation if there is an atom in the target not present in the source" in:
    implicit val transmute: Transmute[List[String], String] = Transmute.atomize[List, String, String](_.mkString(" "))
    val m = Mixer[Boolean *: Int *: List[String] *: EmptyTuple, String *: Int *: EmptyTuple]
    m.mix(true *: 17 *: List("hello", "world") *: EmptyTuple) shouldBe "hello world" *: 17 *: EmptyTuple

  it should "use multiple transmutation if there are atoms in the target not present in the source" in:
    implicit val transmute1: Transmute[Int, Boolean] = Transmute[Int, Boolean](_ % 2 == 0)
    implicit val transmute2: Transmute[List[String], Option[String]] = Transmute.transmuteK[List, Option, String, String](_.headOption)
    val m = Mixer[String *: Int *: List[String] *: EmptyTuple, Option[String] *: Boolean *: EmptyTuple]
    m.mix("bob" *: 17 *: Nil *: EmptyTuple) shouldBe None *: false *: EmptyTuple

  it should "work on nested structures taking left-most atom in cases of ambiguity" in:

    val a1: A1 = A1(5, "s")
    val a2: A2 = A2("s", 5)
    val b: B = B(true, "s")
    val n1: N1 = N1(a1, a2)
    val n2: N2 = N2(a1, b)

    Mixer[(A1, A2), (A1, A2)].mix(a1 -> a2) shouldBe (a1 -> a2)

    Mixer[(A1, A2), A2 *: A1 *: EmptyTuple].mix(a1 -> a2) shouldBe a2 *: a1 *: EmptyTuple

    // Multiple Int and String present so it is ambiguous. It picks the left-most ones
    // String from A1, Int from the outer tuple
    Mixer[(Int, A1, String), A2 *: A1 *: EmptyTuple].mix((17, a1, "string")) shouldBe A2("s", 17) *: A1(17, "s") *: EmptyTuple

    // Constructs both A1 and A2
    Mixer[(Int, String), A2 *: A1 *: EmptyTuple].mix(5 -> "s") shouldBe a2 *: a1 *: EmptyTuple

    Mixer[(Int, Int, String), A2 *: A1 *: EmptyTuple].mix((5, 100, "s")) shouldBe a2 *: a1 *: EmptyTuple

    Mixer[N1, A1 *: A2 *: EmptyTuple].mix(n1) shouldBe a1 *: a2 *: EmptyTuple

    Mixer[(A2, Boolean *: EmptyTuple), N2].mix((a2, true *: EmptyTuple)) shouldBe n2

    Mixer[(Int, Boolean *: String *: EmptyTuple), N2].mix((5, true *: "s" *: EmptyTuple)) shouldBe n2

  it should "work on nested structures obeying custom Atom rules" in:

    implicit val atomA1: Atom[A1] = Atom[A1]

    val a1: A1 = A1(5, "s")

    // A1 is now atomic, so the A1 on LHS migrates to RHS untouched
    // This also makes A1's inner string inaccessible, so the RHS A2 picks up the other string
    // Compare with case in previous test block
    Mixer[(Int, A1, String), A2 *: A1 *: EmptyTuple].mix((17, a1, "string2")) shouldBe A2("string2", 17) *: a1 *: EmptyTuple
    Mixer[(A1, String, Int), A2 *: A1 *: EmptyTuple].mix((a1, "string2", 17)) shouldBe A2("string2", 17) *: a1 *: EmptyTuple
    Mixer[(Int, String, A1), A2 *: A1 *: EmptyTuple].mix((17, "string2", a1)) shouldBe A2("string2", 17) *: a1 *: EmptyTuple

  // See bottom of file for the value classes included below
  // Tree 1
  case class Person(firstName: FirstName, lastName: LastName, age: Age)
  case class Address(a1: Address1, a2: Address2, c: City, p: Postcode)
  case class Resident(p: Person, a: Address)

  // Tree 2
  case class FullName(f: FirstName, l: LastName)
  case class PersonAndPostcode(f: FullName, p: Postcode)
  case class PersonAndPostcodeAndAddress(pp: PersonAndPostcode, a1: Address1, a2: Address2, c: City)

  it should "work in a very complex case" in:

    // Without these all the strings would be equal to the first string found, ie "Boaty"
    implicit val a1: Atom[Age] = Atom[Age]
    implicit val a2: Atom[FirstName] = Atom[FirstName]
    implicit val a3: Atom[LastName] = Atom[LastName]
    implicit val a4: Atom[Address1] = Atom[Address1]
    implicit val a5: Atom[Address2] = Atom[Address2]
    implicit val a6: Atom[City] = Atom[City]
    implicit val a7: Atom[Postcode] = Atom[Postcode]

    val resident = Resident(
      Person(
        FirstName("Boaty"),
        LastName("McBoatface"),
        Age(2)
      ),
      Address(
        Address1("North Pole"),
        Address2("The Arctic"),
        City("Northern Hemisphere"),
        Postcode("N0RT4")
      )
    )

    val expectedResult: PersonAndPostcodeAndAddress = PersonAndPostcodeAndAddress(
      PersonAndPostcode(
        FullName(
          FirstName("Boaty"),
          LastName("McBoatface")
        ),
        Postcode("N0RT4")
      ),
      Address1("North Pole"),
      Address2("The Arctic"),
      City("Northern Hemisphere")
    )

    // No evidence for Age in LHS so can't construct RHS
    illTyped("Mixer[PersonAndPostcodeAndAddress, Resident]")

    Mixer[Resident, PersonAndPostcodeAndAddress].mix(resident) shouldBe expectedResult

  it should "demonstrate behaviour without the expected Atoms present" in:
    case class Age(i: Int)
    case class FirstName(value: String)
    case class LastName(value: String)
    case class Address1(value: String)
    case class Address2(value: String)
    case class City(value: String)
    case class Postcode(value: String)
    case class Person(firstName: FirstName, lastName: LastName, age: Age)
    case class Address(a1: Address1, a2: Address2, c: City, p: Postcode)
    case class Resident(p: Person, a: Address)

    // Tree 2
    case class FullName(f: FirstName, l: LastName)
    case class PersonAndPostcode(f: FullName, p: Postcode)
    case class PersonAndPostcodeAndAddress(pp: PersonAndPostcode, a1: Address1, a2: Address2, c: City)

    val resident = Resident(
      Person(
        FirstName("Boaty"),
        LastName("McBoatface"),
        Age(2)
      ),
      Address(
        Address1("North Pole"),
        Address2("The Arctic"),
        City("Northern Hemisphere"),
        Postcode("N0RT4")
      )
    )

    // There are no atoms for our types, so we expect every string to be filled from the first encountered
    val expectedResult: PersonAndPostcodeAndAddress = PersonAndPostcodeAndAddress(
      PersonAndPostcode(
        FullName(
          FirstName("Boaty"),
          LastName("Boaty")
        ),
        Postcode("Boaty")
      ),
      Address1("Boaty"),
      Address2("Boaty"),
      City("Boaty")
    )

    Mixer[Resident, PersonAndPostcodeAndAddress].mix(resident) shouldBe expectedResult

  it should "not compile if there is no Atom at all present" in:
    trait TestTrait

    case class Z(a: Int, b: TestTrait)

    // No Atom[TestTrait] so this does not compile
    illTyped("Mixer[Z, (Int, TestTrait)]")

  it should "work with a brand new type and supplied Atom" in:
    trait TestTrait
    case object Testing extends TestTrait

    implicit val atom: Atom[TestTrait] = Atom[TestTrait]

    case class Z(a: Int, b: TestTrait)

    // No Atom[TestTrait] so this does not compile
    Mixer[Z, (Int, TestTrait)].mix(Z(5, Testing)) shouldBe 5 -> Testing

  "Mixer on molecules" should "mix up to the boundaries of molecules, and then isolated within those molecules" in:

    case class A(i: Int, b: Boolean)
    case class B(i: String)
    case class Source(i: Int, s: String, as: List[A], bs: List[B])

    case class BS(bs: List[B])
    case class Target(i: Int, bs: BS, as: List[(Boolean, Int)])

    val mixer = Mixer[Source, Target]

    val a1 = A(1, true)
    val a2 = A(2, false)
    val a3 = A(3, true)

    val b1 = B("ten")
    val b2 = B("twenty")

    val source = Source(17, "DANGER", List(a1, a2, a3), List(b1, b2))
    val target = Target(17, BS(List(b1, b2)), List(true -> 1, false -> 2, true -> 3))

    mixer.mix(source) shouldBe target

  it should "allow custom sub-mixers to be injected, ignoring the default implicits found" in:
    case class A(i: Int, b: Boolean)
    case class Source(as: List[A])
    case class Target(as: List[(Boolean, Int)])

    // A can be Mixed to (Boolean, Int) automatically, but we're overriding that
    // behaviour with our own
    implicit val mixer: Mixer[A, (Boolean, Int)] = new Mixer[A, (Boolean, Int)]:
      def mix(a: A): (Boolean, Int) = a.b -> (a.i + 100)

      def inject(b: (Boolean, Int), a: A): A = a

    val source = Source(List(A(1, true), A(2, false), A(3, true)))
    val target = Target(List((true, 101), (false, 102), (true, 103)))
    Mixer[Source, Target].mix(source) shouldBe target

  it should "work for a medium sized case class with a medium sized sub molecule" in:

    case class X(
      a: Int,
      b: String,
      c: Boolean,
      l: List[(Int, String, Boolean, Float, Char)])

    case class Y(
      i: Int,
      b: String,
      c: Boolean,
      l: List[(Float, Char)]
    )

    val x = X(5, "2", true, List((0, "0", false, 7.6f, 'f')))
    val y = Y(5, "2", true, List((7.6f, 'f')))

    (Mixer[X, Y].mix(x): Y) shouldBe y

  it should "work with case classes in molecules" in:

    case class Source(i: Int, b: Boolean)
    case class Target(i: Int, b: Boolean)

    case class A(sources: List[Source])
    case class B(targets: List[Target])

    Mixer[A, B]

  it should "be able to use an atom for a type with a molecule instance" in:
    class Foo
    type A = Vector[String]

    import cats.implicits.catsStdInstancesForVector
    implicit val mol: Molecule[Vector, String] = Molecule[Vector, String]

    // if there is an atom in scope it uses the atom for mixing
    val _ =
      implicit val atom: Atom[A] = Atom[A]
      val mixer = Mixer[(A, Boolean), Boolean *: A *: EmptyTuple]
      mixer.mix((Vector("hi", "there"), true)) shouldBe true *: Vector("hi", "there") *: EmptyTuple

    val _1 =
      implicit val atom: DefaultAtom[A] = DefaultAtom[A](Vector.empty[String])
      val mixer = Mixer[Boolean *: EmptyTuple, Boolean *: A *: EmptyTuple]
      mixer.mix(true *: EmptyTuple) shouldBe true *: Vector.empty[String] *: EmptyTuple

    //if there is no atom in scope it uses the molecule for mixing
    val mixer = Mixer[(A, Boolean), Boolean *: A *: EmptyTuple]
    mixer.mix((Vector("hi", "there"), true)) shouldBe true *: Vector("hi", "there") *: EmptyTuple

  "Mixer with defaults" should "supply a default value" in:
    case class A(i: Int, b: Boolean, c: Char)
    case class B(i: Int)

    val mixer: Mixer[B, A] = Mixer.from[B].to[A].withDefault(true).withDefault('d').build
    mixer.mix(B(5)) shouldBe A(5, true, 'd')

  it should "be able to be implicitly passed through molecules into sub searches" in:
    case class A(i: Int, b: Boolean)
    case class Source(i: Int, s: String, as: List[A])
    case class Target(i: Int, as: List[(Boolean, Int, Char)], c: Char)

    implicit val aMixer: Mixer[A, (Boolean, Int, Char)] = Mixer.from[A].to[(Boolean, Int, Char)].withDefault('e').build
    val mixer = Mixer.from[Source].to[Target].withDefault('f').build

    // The 'e' has been inserted in the list, and the 'f' on the outside - different defaults for different mixers
    val target = Target(17, List((true, 1, 'e'), (false, 2, 'e'), (true, 3, 'e')), 'f')
    val source = Source(17, "DANGER", List(A(1, true), A(2, false), A(3, true)))

    mixer.mix(source) shouldBe target

  "Mixer" should "combine everything into a very complex test" in:
    case class FirstName(value: String)
    case class LastName(value: String)
    case class Address1(value: String)
    case class City(value: String)
    case class Postcode(value: String)
    case class Title(value: String)
    case class Gender(value: String)

    // setup
    implicit val a1: Atom[FirstName] = Atom[FirstName]
    implicit val a2: Atom[LastName] = Atom[LastName]
    implicit val a3: Atom[Address1] = Atom[Address1]
    implicit val a4: Atom[City] = Atom[City]
    implicit val a5: Atom[Postcode] = Atom[Postcode]
    implicit val a6: Atom[Title] = Atom[Title]
    implicit val a7: Atom[Gender] = Atom[Gender]

    // This is our data tree
    case class Address(a1: Address1, c: City, p: Postcode)
    case class Alias(firstName: FirstName, lastName: LastName, isLegal: Boolean)
    case class AddressHistory(values: List[Address])

    case class Source(
      firstName: FirstName,
      lastName: LastName,
      addressHistory: AddressHistory,
      aliases: List[(Title, FirstName, LastName)]
    )

    case class Target(
      name: (FirstName, LastName),
      addresses: List[(Address1, Postcode)],
      aliases: List[Alias],
      gender: Gender
    )

    // We're going to map an intance of Source into an instance of Target
    val source = Source(
      firstName = FirstName("John"),
      lastName = LastName("Johnson"),
      addressHistory = AddressHistory(List(
        Address(Address1("5 John Street"), City("Johnsville"), Postcode("JOHN")),
        Address(Address1("5 Jack Street"), City("Jacksville"), Postcode("JACK"))
      )),
      aliases = List(
        (Title("Mr"), FirstName("Johnny"), LastName("Vegas")),
        (Title("Mr"), FirstName("Jon"), LastName("Snow"))
      )
    )

    // We need a Mixer for Alias first to supply the missing Boolean
    implicit val submixer: Mixer[(Title, FirstName, LastName), Alias] = Mixer.from[(Title, FirstName, LastName)].to[Alias].withDefault(true).build
    // Now we can mix the whole structure
    val mixer = Mixer.from[Source].to[Target].withDefault(Gender("male")).build

    val result = mixer.mix(source)

    // This test passes!
    result shouldBe Target(
      name = (FirstName("John"), LastName("Johnson")),
      addresses = List(
        (Address1("5 John Street"), Postcode("JOHN")),
        (Address1("5 Jack Street"), Postcode("JACK"))
      ),
      aliases = List(
        Alias(FirstName("Johnny"), LastName("Vegas"), true),
        Alias(FirstName("Jon"), LastName("Snow"), true)
      ),
      gender = Gender("male")
    )
