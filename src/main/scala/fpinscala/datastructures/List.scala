package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, a) => a
  }

  def head[A](l: List[A]): A = l match {
    case Cons(a, _) => a
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case x if x <= 0 => l
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = sys.error("todo")

  def length[A](l: List[A]): Int = {
    foldRight(l, 0) { (el, acum) => el match {
      case Nil => acum
      case _ => acum + 1
    }
    }
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((z: Int, a: A) => z + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((a, b) =>
    Cons(b, a))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightFromFL(l, Nil:List[B]) ((a,z) => Cons(f(a), z))

  //def foldLeftFromLR[A,B](l: List[A], z: B)(f: (B, A) => B): B =
  //  foldRight(l, z)()

  def foldRightFromFL[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z) ((a, b) => f(b, a))
  }


  def addOne(l: List[Int]): List[Int] =
    foldLeft(reverse(l) , Nil: List[Int])((a, b) => Cons[Int](b + 1, a))

  def doubleToString(l:List[Double]) : List[String] =
    foldRightFromFL(l, Nil: List[String]) ((a, z) => Cons(a.toString, z))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightFromFL(as, Nil: List[A]) ((a,z) => if (f(a)) z else  Cons(a, z))
}