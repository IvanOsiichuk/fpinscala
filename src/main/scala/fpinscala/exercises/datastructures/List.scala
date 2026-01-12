package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("the list is empty")
      case Cons(h, t) => t
        }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("the list is empty")
      case Cons(_, t) => Cons(h, t)
    }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case xs if n <= 0 => xs
      case Cons(_, t) => drop(t, n - 1)
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case c@Cons(h, t) =>
        if f(h) then dropWhile(t, f)
        else c
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("the list is empty")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    l match {
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)
    }

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (acc, x) => Cons(x, acc))

  def foldRightViaLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (x, acc) => Cons(x, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], (x, acc) => appendViaFoldRight(x, acc))

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int], (x, acc) => Cons(x + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (x, acc) => Cons(x.toString, acc))

  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRight(l, Nil: List[B], (x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A], (x, acc) => if f(x) then Cons(x, acc) else acc)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B], (x, acc) => append(f(x), acc))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, x => if f(x) then List(x) else Nil)

  def combine[A, B, C](a: List[A], b: List[B], cF: (A, B) => C): List[C] =
    (a, b) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(cF(h1, h2), combine(t1, t2, cF))
      case (Nil, _) | (_, Nil) => Nil
    }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    combine(a, b, (x, y) => x + y)

  def zip[A, B](a: List[A], b: List[B]): List[(A, B)] =
    combine(a, b, (x, y) => (x, y))

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match {
      case (_, Nil) => true
      case (Nil, _) => prefix == Nil
      case (Cons(h, t), Cons(ph, pt)) =>
        if h == ph then startsWith(t, pt)
        else false
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case Cons(_, t) => startsWith(sup, sub) || hasSubsequence(t, sub)
    }

  def hasSubsequenceStandalone[A](sup: List[A], sub: List[A]): Boolean =
    @annotation.tailrec
    def loop(xs: List[A], seq: List[A]): Boolean =
      (xs, seq) match {
        case (_, Nil) => true
        case (Nil, _) => seq == Nil
        case (Cons(h, t), Cons(sh, st)) =>
          loop(
            t,
            if h == sh then st else sub
          )
      }

    loop(sup, sub)
