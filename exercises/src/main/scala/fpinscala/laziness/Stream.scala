package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toListRecursive: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) =>
        buf += h()
        go(t())
    }
    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
    case (Cons(h, _), 1) => Some((h(), (empty, 0)))
    case _ => None
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A]) {
    case (h, t) => if (p(h)) cons(h, t) else empty
  }

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h) )

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: => (A => B)): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))
  def filter(p: => (A => Boolean)): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)
  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))
  def flatMap[B](f: => (A => Stream[B])): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  def mapViaUnfold[B](f: => (A => B)): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def zipWith[B](s: Stream[B]): Stream[(A, B)] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.isDefined) forAll {
    case (h, hs) => h == hs
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s @ Cons(_, t) => Some((s, t()))
    case _ => None
  } append empty

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, results) => {
      lazy val r = results
      val b = f(a, r._1)
      (b, cons(b, r._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => cons(h, unfold(t)(f))
    case _ => empty
  }

  def fibsViaUnfold: Stream[Int] = unfold((0,1)) {
    case (a, b) => Some((a, (b, a + b)))
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))
  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

}
