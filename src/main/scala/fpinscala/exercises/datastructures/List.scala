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
    l match
      case Nil => throw new Exception("length should not be empty")
      case Cons(head, tail) => tail

  def setHead[A](l: List[A], h: A): List[A] = 
    l match
      case Nil => throw new Exception("length should not be empty")
      case Cons(head, tail) => Cons(h, tail)

  def drop[A](l: List[A], n: Int): List[A] = 
    if n <= 0 then l
    else l match
      case Nil => Nil
      case Cons(head, tail) => drop(tail, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match
      case Nil => Nil
      case Cons(head, tail) =>
        if f(head) then dropWhile(tail, f)
        else l

  def init[A](l: List[A]): List[A] = 
    l match
      case Nil => throw new Exception("l should not be empty")
      case Cons(last, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (a, b) => b + 1)

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = 
    @annotation.tailrec
    def go(l: List[A], current: B): B =
      l match
        case Nil => current
        case Cons(head, tail) => go(tail, f(current, head))
    
    go(l, acc)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = 
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (a, _) => a + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil, (b: List[A], a: A) => Cons(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    l match
      case Nil => Nil
      case Cons(head, tail) => appendViaFoldRight(head, concat(tail))
    
  def incrementEach(l: List[Int]): List[Int] =
    l match
      case Nil => Nil
      case Cons(head, tail) => Cons(head + 1, incrementEach(tail))

  def doubleToString(l: List[Double]): List[String] =
    map(l, _.toString)

  def map[A,B](l: List[A], f: A => B): List[B] =
    l match
      case Nil => Nil
      case Cons(head, tail) => Cons(f(head), map(tail, f))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil => Nil
      case Cons(head, tail) =>
        if f(head) then Cons(head, filter(tail, f))
        else filter(tail, f)
    

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then Cons(a, Nil) else Nil)


  def head[A](l: List[A]): A =
    l match
      case Nil => throw new Exception("Error")
      case Cons(head, tail) => head

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    zipWith(a, b, _ + _)

  // def zipWith - TODO determine signature
  def zipWith[A](a: List[A], b: List[A], f: (A, A) => A): List[A] =
    if a == Nil || b == Nil then Nil
    else Cons(f(head(a), head(b)), zipWith(tail(a), tail(b), f))

  def prefixEqual[A](a: List[A], b: List[A]): Boolean =
    (a, b) match
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => (h1 == h2) && prefixEqual(t1, t2)
    

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match
      case Nil => sub == Nil
      case Cons(head, tail) => prefixEqual(sup, sub) || hasSubsequence(tail, sub)
