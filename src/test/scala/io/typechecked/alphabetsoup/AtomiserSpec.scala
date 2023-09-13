package io.typechecked.alphabetsoup

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AtomiserSpec extends AnyFlatSpec with Matchers {

  "Atomiser" should "work on Unit" in {
    val g = Atomiser[Unit]
    (g.to(()): Unit) shouldBe (())
    (g.from(()): Unit) shouldBe (())
  }

  it should "work on EmptyTuple" in {
    val g = Atomiser[EmptyTuple]
    (g.to(EmptyTuple): EmptyTuple) shouldBe EmptyTuple
    (g.from(EmptyTuple): EmptyTuple) shouldBe EmptyTuple
  }

  it should "work on simple hlists" in {
    type H = Int *: String *: Boolean *: EmptyTuple
    val hlist: H = 5 *: "hello" *: true *: EmptyTuple

    Atomiser[Int *: EmptyTuple]
    val gen = Atomiser[H]
    (gen.to(hlist): H) shouldBe hlist
    (gen.from(hlist): H) shouldBe hlist
  }

  it should "work on complex hlists" in {
    type H = Int *: (String *: Boolean *: EmptyTuple) *: (Float *: Double *: EmptyTuple) *: Boolean *: EmptyTuple
    val hlist: H = 5 *: ("hello" *: true *: EmptyTuple) *: (3.4f *: 3.9 *: EmptyTuple) *: true *: EmptyTuple
    val gen = Atomiser[H]
    (gen.to(hlist): H) shouldBe hlist
    (gen.from(hlist): H) shouldBe hlist
  }

  it should "work with small tuples" in {
    type T = (String, Boolean)
    type H = String *: Boolean *: EmptyTuple
    val gen = Atomiser[T]
    val tuple: T = ("hello", true)
    val hlist: H = "hello" *: true *: EmptyTuple

    (gen.to(tuple): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe tuple
  }

  it should "work with a larger flat tuple" in {
    type T = (Int, String, Boolean)
    type H = Int *: String *: Boolean *: EmptyTuple
    val gen = Atomiser[T]
    val tuple: T = (5, "hello", true)
    val hlist: H = 5 *: "hello" *: true *: EmptyTuple

    (gen.to(tuple): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe tuple
  }

  it should "work on flat case classes" in {
    case class C(i: Int, s: String, b: Boolean)
    type H = Int *: String *: Boolean *: EmptyTuple
    val gen = Atomiser[C]
    val value: C = C(5, "hello", true)
    val hlist: H = 5 *: "hello" *: true *: EmptyTuple
    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): C) shouldBe value
  }

  it should "work for simply nested tuples recursively" in {
    type T = ((Int, Float), String, Boolean)
    type H = (Int *: Float *: EmptyTuple) *: String *: Boolean *: EmptyTuple
    val gen = Atomiser[T]
    val tuple: T = ((5, 4.5f), "hello", true)
    val hlist: H = (5 *: 4.5f *: EmptyTuple) *: "hello" *: true *: EmptyTuple

    (gen.to(tuple): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe tuple
  }

  it should "work for simply nested case classes recursively" in {
    case class N(i: Int, f: Char)
    case class T(n: N, s: String, b: Boolean)

    type H = (Int *: Char *: EmptyTuple) *: String *: Boolean *: EmptyTuple

    val gen = Atomiser[T]
    val value: T = T(N(5, 'g'), "hello", true)
    val hlist: H = (5 *: 'g' *: EmptyTuple) *: "hello" *: true *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe value
  }

  it should "work for complex nested tuples recursively" in {
    type T = ((Int, Float), (String, (Boolean, Double)))
    type H = (Int *: Float *: EmptyTuple) *: (String *: (Boolean *: Double *: EmptyTuple) *: EmptyTuple) *: EmptyTuple
    val gen = Atomiser[T]
    val tuple: T = ((5, 4.5f), ("hello", (true, 10.0)))
    val hlist: H = (5 *: 4.5f *: EmptyTuple) *: ("hello" *: (true *: 10.0 *: EmptyTuple) *: EmptyTuple) *: EmptyTuple

    (gen.to(tuple): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe tuple
  }

  it should "work for complex nested case classes recursively" in {
    case class N(i: Int, f: Float)
    case class M(d: Double, n: N)
    case class P(b: Boolean, l: Long)
    case class T(n: N, p: P, m: M, s: String)

    type H = (Int *: Float *: EmptyTuple) *: (Boolean *: Long *: EmptyTuple) *: (Double *: (Int *: Float *: EmptyTuple) *: EmptyTuple) *: String *: EmptyTuple

    val gen = Atomiser[T]
    val value: T = T(N(1, 1.4f), P(true, 3L), M(2.3, N(10, 10.4f)), "hello")
    val hlist: H = (1 *: 1.4f *: EmptyTuple) *: (true *: 3L *: EmptyTuple) *: (2.3 *: (10 *: 10.4f *: EmptyTuple) *: EmptyTuple) *: "hello" *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe value
  }


  it should "work for an hlist inside a tuple inside a cas class" in {
    case class T(t: (Double, String *: EmptyTuple))

    type H = (Double *: (String *: EmptyTuple) *: EmptyTuple) *: EmptyTuple

    val gen = Atomiser[T]
    val value: T = T((5.0, "hello" *: EmptyTuple))
    val hlist: H = (5.0 *: ("hello" *: EmptyTuple) *: EmptyTuple) *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe value
  }

  it should "work for a simple case class at the head of an HList" in {
    case class N(t: String)

    type T = N *: Int *: EmptyTuple
    type H = (String *: EmptyTuple) *: Int *: EmptyTuple

    val gen = Atomiser[T]
    val value: T = N("hello") *: 11 *: EmptyTuple
    val hlist: H = ("hello" *: EmptyTuple) *: 11 *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe value
  }

  it should "work for a simple case class at the head of a tuple" in {
    case class N(t: String)

    type T = (N, Int)
    type H = (String *: EmptyTuple) *: Int *: EmptyTuple

    val gen = Atomiser[T]
    val value: T = N("hello") -> 11
    val hlist: H = ("hello" *: EmptyTuple) *: 11 *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe value
  }

  it should "work for a tuple at the head of an hlist at the head of an hlist" in {
    type T = ((String, Boolean)*: EmptyTuple) *: Int *: EmptyTuple
    type H = ((String *: Boolean *: EmptyTuple) *: EmptyTuple) *: Int *: EmptyTuple

    val gen = Atomiser[T]
    val value: T = (("hello" -> true) *: EmptyTuple) *: 11 *: EmptyTuple
    val hlist: H = (("hello" *: true *: EmptyTuple) *: EmptyTuple) *: 11 *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe value
  }

  it should "work for a case class containing only a tuple" in {

    case class N(t: (String, Boolean))
    type H = (String *: Boolean *: EmptyTuple) *: EmptyTuple

    val gen = Atomiser[N]
    val value = N("hello" -> true)
    val hlist = ("hello" *: true *: EmptyTuple) *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): N) shouldBe value
  }

  it should "work for a tuple1 in a case class at the head of a tuple" in {
    case class N(t: Tuple1[String])

    type T = (N, Int)
    type H = ((String *: EmptyTuple) *: EmptyTuple) *: Int *: EmptyTuple

    val gen = Atomiser[T]
    val value: T = N(Tuple1("hello")) -> 11
    val hlist: H = (("hello" *: EmptyTuple) *: EmptyTuple) *: 11 *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe value
  }

  it should "work for an hlist inside a tuple inside a cas class inside a tuple" in {
    case class N(t: (Double, String *: EmptyTuple))

    type T = (N, Int)
    type H = ((Double *: (String *: EmptyTuple) *: EmptyTuple) *: EmptyTuple) *: Int *: EmptyTuple

    val gen = Atomiser[T]
    val value: T = (N((5.0, "hello" *: EmptyTuple)), 11)
    val hlist: H = ((5.0 *: ("hello" *: EmptyTuple) *: EmptyTuple) *: EmptyTuple) *: 11 *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe value
  }

  it should "work for an HList inside a tuple inside a case class inside a tuple inside an HList" in {
    case class N(t: (Double, String *: EmptyTuple))

    type T = Boolean *: Long *: (N, Int) *: EmptyTuple
    type H = Boolean *: Long *: (((Double *: (String *: EmptyTuple) *: EmptyTuple) *: EmptyTuple) *: Int *: EmptyTuple) *: EmptyTuple

    val gen = Atomiser[T]
    val value: T = true *: 10L *: (N((5.0, "hello" *: EmptyTuple)), 11) *: EmptyTuple
    val hlist: H = true *: 10L *: (((5.0 *: ("hello" *: EmptyTuple) *: EmptyTuple) *: EmptyTuple) *: 11 *: EmptyTuple) *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): T) shouldBe value
  }

  it should "work for a complex mix of nested hlists, case classes and tuples" in {

    case class N(t: (Double, String *: EmptyTuple))
    case class C(
      t: (Int, String),
      d: Double,
      h: Boolean *: Long *: (N, Int) *: EmptyTuple
    )

    type H =
      (Int *: String *: EmptyTuple) *:
      Double *:
      (Boolean *: Long *: (((Double *: (String *: EmptyTuple) *: EmptyTuple) *: EmptyTuple) *: Int *: EmptyTuple) *: EmptyTuple) *:
      EmptyTuple

    val gen = Atomiser[C]
    val value = C((5, "hello"), 10.9, true *: 9L *: (N((10.12, "hello2" *: EmptyTuple)), 4) *: EmptyTuple)
    // This commented out behaviour is correct
    val hlist = (5 *: "hello" *: EmptyTuple) *: 10.9 *: (true *: 9L *: (((10.12 *: ("hello2" *: EmptyTuple) *: EmptyTuple) *: EmptyTuple) *: 4 *: EmptyTuple) *: EmptyTuple) *: EmptyTuple

    (gen.to(value): H) shouldBe hlist
    (gen.from(hlist): C) shouldBe value
  }

  it should "stop atomising at atoms" in {
    case class Pair(i: Int, s: String)
    case class T(p: Pair, b: Boolean)
    implicit val pairAtom: Atom[Pair] = Atom[Pair]
    val t = T(Pair(5, "hello"), true)

    (Atomiser[T].to(t): Pair *: Boolean *: EmptyTuple) shouldBe Pair(5, "hello") *: true *: EmptyTuple
  }

  it should "stop processing at molecule boundaries" in {
    case class A(b: Boolean, s: String)
    case class B(i: Int, l: List[A])

    val gen = Atomiser[B]

    type Output = Int *: List[A] *: EmptyTuple
    val value = B(5, List(A(true, "1"), A(false, "2")))
    val hlist = 5 *: List(A(true, "1"), A(false, "2")) *: EmptyTuple
    (gen.to(value): Output) shouldBe hlist
    (gen.from(hlist): B) shouldBe value
  }

  it should "work with an atom and a molecule in scope" in {
    import cats.implicits.catsStdInstancesForVector
    implicit val mol : Molecule[Vector, String] = Molecule[Vector, String]
    implicit val atom: Atom[Vector[String]] = Atom[Vector[String]]

    case class A(value: Vector[String])

    val gen = Atomiser[A]

    (gen.to(A(Vector("hi"))): Vector[String] *: EmptyTuple) shouldBe Vector("hi") *: EmptyTuple
  }

  it should "atomise simple things correctly" in {

    //import macros.Atomic

    class A
    given aAtom: Atom[A] = new Atom[A] {}

    class B
    given bAtom: Atom[B] = new Atom[B] {}

    val a = new A
    val b = new B

    Atomiser[A].to(a) shouldBe a
    Atomiser[B].to(b) shouldBe b
    Atomiser[(A, B)].to(a -> b) shouldBe a *: b *: EmptyTuple

    trait C
    given cAtom: Atom[C] = new Atom[C] {}

    trait D
    given dAtom: Atom[D] = new Atom[D] {}

    val c = new C {}
    val d = new D {}

    Atomiser[C].to(c) shouldBe c
    Atomiser[D].to(d) shouldBe d
    Atomiser[(C, D)].to(c -> d) shouldBe c *: d *: EmptyTuple

    case class E()
    given eAtom: Atom[E] = new Atom[E] {}
    case class F()
    given fAtom: Atom[F] = new Atom[F] {}

    val e = E()
    val f = F()

    Atomiser[E].to(e) shouldBe e
    Atomiser[F].to(f) shouldBe f
    Atomiser[(E, F)].to(e -> f) shouldBe (e *: f *: EmptyTuple)

  }

}
