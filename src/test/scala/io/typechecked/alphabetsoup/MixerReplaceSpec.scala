package io.typechecked.alphabetsoup

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MixerReplaceSpec extends AnyFlatSpec with Matchers:

  "Mixer.inject" should "work for simple types" in:
    case class Source(a: Int)
    case class Target(b: Int)

    Mixer[Source, Target].inject(Target(7), Source(1)) shouldBe Source(7)

  it should "work for nested types" in:
    case class Inner(i: Int)
    given innerAtom: Atom[Inner] = new Atom[Inner] {}

    case class Source(i: Inner)
    case class Target(i: Inner)

    Mixer[Source, Target].inject(Target(Inner(7)), Source(Inner(12))) shouldBe Source(Inner(7))

  it should "replace first instance of the atoms found" in:
    case class MostInner(s: String)
    case class Inner1(a: MostInner)
    case class Inner2(a: MostInner)
    case class Source(a: Inner1, b: Inner2)

    case class Target(s: MostInner)

    val source = Source(Inner1(MostInner("LOOK AT ME!")), Inner2(MostInner("I'M OVER HERE")))

    val target = Target(MostInner("Nah"))

    Mixer[Source, Target].inject(target, source) shouldBe Source(Inner1(MostInner("Nah")), Inner2(MostInner("I'M OVER HERE")))

  it should "replace multiple different atoms in complex structures" in:
    case class MostInner(s: String)
    case class Inner1(a: Int)
    case class Inner2(a: MostInner)
    case class Source(a: Inner1, b: Inner2)

    case class Target(s: MostInner, i: Int)

    val source = Source(Inner1(17), Inner2(MostInner("I'M OVER HERE")))

    val target = Target(MostInner("Nah"), -20)

    Mixer[Source, Target].inject(target, source) shouldBe Source(Inner1(-20), Inner2(MostInner("Nah")))

  it should "edit molecules if they are isomorphic" in:
    case class A(i: Int, b: Boolean)
    case class B(i: String)
    case class Source(i: Int, s: String, as: List[A], bs: List[B])

    case class BS(bs: List[B])
    case class Target(i: Int, bs: BS, as: List[(Boolean, Int)])

    val a1 = A(1, true)
    val a2 = A(2, false)
    val a3 = A(3, true)

    val b1 = B("ten")
    val b2 = B("twenty")

    val source = Source(17, "DANGER", List(A(0, false), A(0, true), A(0, false)), List(B("NOOO"), B("NOT MEEEEE")))
    val target = Target(7, BS(List(b1, b2)), List(true -> 1, false -> 2, true -> 3))

    Mixer[Source, Target].inject(target, source) shouldBe Source(7, "DANGER", List(A(1,true), A(2,false), A(3,true)), List(b1, b2))

  it should "not work for molecules where they are not isomorphic" in:
    case class A(i: Int, b: Boolean)
    case class B(i: String)
    case class Source(i: Int, s: String, as: List[A], bs: List[B])

    case class BS(bs: List[B])
    case class Target(i: Int, bs: BS, as: List[(Boolean)])

    val a1 = A(1, true)
    val a2 = A(2, false)
    val a3 = A(3, true)

    val b1 = B("ten")
    val b2 = B("twenty")

    val source = Source(17, "DANGER", List(A(0, false), A(0, true), A(0, false)), List(B("NOOO"), B("NOT MEEEEE")))
    val target = Target(7, BS(List(b1, b2)), List(true, false, true))

    // Note the internal molecule: Just uses source's value because the target doesn't have a molecule which can
    // project onto it
    // We have a Mixer[A, Boolean] but not the other way
    Mixer[Source, Target].inject(target, source) shouldBe Source(7, "DANGER", source.as, List(b1, b2))

  it should "work when values are the same" in:
    case class Source(a: Int)
    case class Target(b: Int)

    Mixer[Source, Target].inject(Target(1), Source(1)) shouldBe Source(1)

  it should "not work if target is not wholly contained within source" in:
    case class Source(a: Int)
    case class Target(b: Int, c: String)

    illTyped("Mixer[Source, Target].inject(Target(1), Source(1)) shouldBe Source(1)")

