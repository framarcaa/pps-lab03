package u03

object Streams extends App :

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    //6
    def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
      case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => Empty()

    //7
    def fill[A](n: Int)(k: A): Stream[A] = n match
      case 0 => Empty()
      case _ => Cons(() => k, () => fill(n-1)(k))

    //9
    def fromList[A](l: List[A]): Stream[A] = l match
      case Nil => Empty()
      case head :: tail => cons(head, fromList(tail))

    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = (s1, s2) match
      case (Cons(h1, t1), Cons(h2, t2)) => cons(h1(), cons(h2(), interleave(t1(), t2())))
      case (Empty(), s) => s
      case (s, Empty()) => s

    //10
    def cycle[A](l: Sequence[A]): Stream[A] =
      def loop(curr: Sequence[A], original: Sequence[A]): Stream[A] = curr match
        case u03.Sequences.Sequence.Cons(h, t) => cons(h, loop(t, original))
        case u03.Sequences.Sequence.Nil() => loop(original, original)
      l match
        case u03.Sequences.Sequence.Nil() => Empty()
        case _ => loop(l,l)

  end Stream

@main def tryStreams =
  import Streams.* 

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]

@main def tryNewStreamFunc =
  import Streams.*
  import Sequences.Sequence.*

  val stream = Stream.iterate(0)(_ + 1)
  println(Stream.toList(Stream.takeWhile(stream)(_ < 5))) // Cons (0 , Cons (1 , Cons (2 , Cons (3 , Cons (4 , Nil ())))))

  println(Stream.toList(Stream.fill(3)("a")))  // Cons (a Cons (a, Cons (a, Nil ())))

  //8
  def getFibonacci(n: Int): Int = n match
    case 0 => 0
    case 1 => 1
    case _ => getFibonacci(n - 1) + getFibonacci(n - 2)

  val fibonacci: Stream[Int] = Stream.map(Stream.iterate(0)(_ + 1))(getFibonacci)
  println(Stream.toList(Stream.take(fibonacci)(5))) // Cons (0 , Cons (1 , Cons (1 , Cons (2 , Cons (3 , Nil ()))))

  //9
  val s1 = Stream.fromList(List(1, 3, 5))
  val s2 = Stream.fromList(List(2, 4, 6, 8, 10))
  println(Stream.toList(Stream.interleave(s1, s2)))

  //10
  val repeat = Stream.cycle(Cons('a', Cons('b', Cons('c', Nil()))))
  println(Stream.toList(Stream.take(repeat)(5)))