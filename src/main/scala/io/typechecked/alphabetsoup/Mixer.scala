package io.typechecked.alphabetsoup

// This is the version application code should use
trait Mixer[A, B]:
  def mix(a: A): B
  def inject(b: B, a: A): A
  def modify(f: B => B)(a: A): A = inject(f(mix(a)), a)

object Mixer:

  def apply[A, B](using m: MixerImpl[A, B]): Mixer[A, B] = fromMixerImpl(m)

  given materialise[A, B](using m: MixerImpl[A, B]): Mixer[A, B] = fromMixerImpl(m)

  def from[From]: MixerBuilder[From] = new MixerBuilder[From] {}

  trait MixerBuilder[From]:

    def to[To]: MixerBuilderCanComplete[EmptyTuple, To] = MixerBuilderCanComplete[EmptyTuple, To](EmptyTuple)

    case class MixerBuilderCanComplete[Defaults <: Tuple, To](defaults: Defaults):

      def withDefault[F](t: F): MixerBuilderCanComplete[F *: Defaults *: EmptyTuple, To] =
        MixerBuilderCanComplete[F *: Defaults *: EmptyTuple, To](t *: this.defaults *: EmptyTuple)

      def mix(from: From)(using m: MixerImpl[From *: Defaults *: EmptyTuple, To]): To =
        m.mix(from *: defaults *: EmptyTuple)

      def build(using m: MixerImpl[From *: Defaults *: EmptyTuple, To], r: MixerImpl[(To, From), From]): Mixer[From, To] =
        new Mixer[From, To]:
          def mix(a: From): To = m.mix(a *: defaults *: EmptyTuple)
          def inject(b: To, a: From): From = r.mix(b -> a)



  private def fromMixerImpl[A, B](m: MixerImpl[A, B]): Mixer[A, B] =
    new Mixer[A, B]:
      def mix(a: A): B = m.mix(a)
      def inject(b: B, a: A): A = m.inject(b, a)

/**
 * Mixes A into B, atomising them both as a first step
 *
 * Allows you to inject an instance of B into an instance of A, replacing all values in A whose types
 * have a corresponding value in B
 *
 * MixerImplFromAtomised[A, B].inject(b, a) is the same as MixerImplFromAtomised[(B, A), A].mix(b -> a)
 * but more efficient due to reusing the same internal structures
 */
trait MixerImpl[A, B]:
  def mix(a: A): B
  def inject(b: B, a: A): A

object MixerImpl:

  def apply[A, B](using m: MixerImpl[A, B]): MixerImpl[A, B] = m

  given mixerImplEquality[A]: MixerImpl[A, A] = new MixerImpl[A, A]:
    def mix(a: A): A = a
    def inject(b: A, a: A): A = b

  // Do not put bound `BOut <: Tuple`
  // It does not behave as expected and inference causes issues downstream
  // Reply on boutIsTuple check instead
  given atomiseThenImpl[A, AOut, B, BOut](using
    atomiserB: Atomiser.Aux[B, BOut],
    boutIsTuple: BOut <:< Tuple,
    atomiserA: Atomiser.Aux[A, AOut],
    m: => MixerImplFromAtomised[AOut, BOut]
  ): MixerImpl[A, B] = new MixerImpl[A, B]:
    def mix(a: A): B = atomiserB.from(m.mix(atomiserA.to(a)))
    def inject(b: B, a: A): A =
      val bAtoms = atomiserB.to(b)
      val aAtoms = atomiserA.to(a)
      val injected = m.inject(bAtoms, aAtoms)
      atomiserA.from(injected)

  given bIsAtomRecurse[A, B](using
    atom: Atom[B],
    s: AtomSelector[A, B]
  ): MixerImpl[A, B] = new MixerImpl[A, B]:
    def mix(a: A): B = s(a)
    def inject(b: B, a: A): A = s.replace(b, a)

  given bIsMoleculeRecurse[A, M[_], B](using
    molecule: Molecule[M, B],
    s: AtomSelector[A, M[B]]
  ): MixerImpl[A, M[B]] = new MixerImpl[A, M[B]]:
    def mix(a: A): M[B] = s(a)
    def inject(b: M[B], a: A): A = s.replace(b, a)


/**
 * Mixes A into B, assuming both A and B have been atomised
 *
 * Allows you to inject an instance of B into an instance of A, replacing all values in A whose types
 * have a corresponding value in B
 *
 * MixerImplFromAtomised[A, B].inject(b, a) is the same as MixerImplFromAtomised[(B, A), A].mix(b -> a)
 * but more efficient due to reusing the same internal structures
 */
trait MixerImplFromAtomised[A, B]:
  def mix(a: A): B
  def inject(b: B, a: A): A

object MixerImplFromAtomised extends LowPriorityMFAImplicits1:

  def apply[A, B](using m: MixerImplFromAtomised[A, B]): MixerImplFromAtomised[A, B] = m

  // Anything can satisfy EmptyTuple
  given EmptyTupleCase[A]: MixerImplFromAtomised[A, EmptyTuple] = new MixerImplFromAtomised[A, EmptyTuple]:
    def mix(a: A): EmptyTuple = EmptyTuple
    def inject(b: EmptyTuple, a: A): A = a

  given bHeadIsAtomRecurse[A, BH, BT <: Tuple](using
    atom: Atom[BH],
    s: SelectOrDefaultOrTransmute[A, BH],
    m2: MixerImplFromAtomised[A, BT]
  ): MixerImplFromAtomised[A, BH *: BT] = new MixerImplFromAtomised[A, BH *: BT]:
    def mix(a: A): BH *: BT = s(a) *: m2.mix(a)
    def inject(b: BH *: BT, a: A): A =
      val tailInjected = m2.inject(b.tail, a)
      s.replace(b.head, tailInjected)

  given bHeadIsMoleculeRecurse[A, M[_], BH, BT <: Tuple](using
    molecule: Molecule[M, BH],
    s: SelectOrDefaultOrTransmute[A, M[BH]],
    m2: MixerImplFromAtomised[A, BT]
  ): MixerImplFromAtomised[A, M[BH] *: BT] = new MixerImplFromAtomised[A, M[BH] *: BT]:
    def mix(a: A): M[BH] *: BT = s(a) *: m2.mix(a)
    def inject(b: M[BH] *: BT, a: A): A =
      val tailInjected = m2.inject(b.tail, a)
      s.replace(b.head, tailInjected)


trait LowPriorityMFAImplicits1:

  given bHeadIsTupleRecurse[A, BT <: Tuple, BHH, BHT <: Tuple](using
    m1: => MixerImplFromAtomised[A, BHH *: BHT],
    m2: MixerImplFromAtomised[A, BT]
  ): MixerImplFromAtomised[A, (BHH *: BHT) *: BT] = new MixerImplFromAtomised[A, (BHH *: BHT) *: BT]:
    def mix(a: A): (BHH *: BHT) *: BT = m1.mix(a) *: m2.mix(a)
    def inject(b: (BHH *: BHT) *: BT, a: A): A =
      val tailInjected = m2.inject(b.tail, a)
      m1.inject(b.head, tailInjected)

