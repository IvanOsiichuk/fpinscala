package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{None as _, Option as _, Some as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] =
    fold(None, x => Some(f(x)))
// or
//    this match {
//      case Some(v) => Some(f(v))
//      case None => None
//    }


  def getOrElse[B>:A](default: => B): B =
    fold(default, identity)
// or
//    this match {
//      case Some(v) => v
//      case None => default
//    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)
// or
//    fold(None, f)
// or
//    this match {
//      case Some(v) => f(v)
//      case None => None
//    }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map(Some.apply).getOrElse(ob)
// or
//    fold(ob, Some(_))
// or
//    this match {
//      case x: Some[B] => x
//      case None => ob
//    }

  def filter(f: A => Boolean): Option[A] =
    this match {
      case x@Some(v) if f(v) => x
      case _ => None
    }
// or
//    flatMap(x => if f(x) then Some(x) else None)
// or
//    fold(None, x => if f(x) then Some(x) else None)

  // more generalized version of `map`
  def fold[B](none: B, f: A => B): B =
    this match {
      case Some(v) => f(v)
      case None => none
    }

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      val diffs = xs.map(x => math.pow(x - m, 2))
      mean(diffs)
    }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap { x =>
      b.map(y => f(x, y))
    }
// or
//    for {
//      x <- a
//      y <- b
//    } yield f(x, y)
// or
//    (a, b) match {
//      case (Some(x), Some(y)) => Some(f(x, y))
//      case _ => None
//    }

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as match {
      case h :: t => map2(h, sequence(t))(_ :: _)
      case Nil => Some(Nil)
    }
// or
//    as.foldLeft(Some(Nil: List[A])) { (acc, x) =>
//      map2(acc, x)(_ appended _)
//    }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match {
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
      case Nil => Some(Nil)
      // =
      // case h :: t => f(h).flatMap { x =>
      //   traverse(t)(f).map(xs => x :: xs)
      // }
    }
