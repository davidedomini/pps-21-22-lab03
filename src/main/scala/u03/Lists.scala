package u03
import u02.Optionals.*
import u02.AlgebraicDataTypes.*

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h,t) if n > 0 => drop(t, n-1)
      case Cons(h,t) => Cons(h,t)
      case Nil() => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h,t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h,t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    def mapWithFlatMap[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filterWithFlatMap[A](l1: List[A])(pred: A => Boolean): List[A] =
      val f = (v: A) => pred(v) match
        case true => Cons(v, Nil())
        case _ => Nil()
      flatMap(l1)(f)

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => max(t) match
        case Option.Some(i) if h > i => Option.Some(h)
        case Option.Some(i) => Option.Some(i)
        case Option.None() => Option.Some(h)
      case Nil() => Option.None()

//  A version of max using the orElse method
    def maxWithOrElse(l: List[Int]): Option[Int] = l match
      case Cons(h, t) if h >= Option.orElse(max(t), h) => Option.Some(h)
      case Cons(h, t) => max(t)
      case Nil() => Option.None()

    def getTeacherCourses(l: List[Person]): List[String] =
      map(filter(l)({case Person.Student(n, a) => false case Person.Teacher(n, c) => true}))({case Person.Teacher(n, c) => c})

    def getTeacherCoursesWithFlatMap(l: List[Person]): List[String] =
      flatMap(l)({case Person.Teacher(n, c) => Cons(c, Nil()) case Person.Student(n, a) => Nil()})

    def foldLeft[A, B](l: List[A])(accumulator: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(accumulator,h))(f)
      case Nil() => accumulator

    def reverse[A](l: List[A]): List[A] = l match
      case Cons(h,t) => append(reverse(t), Cons(h, Nil()))
      case Nil() => Nil()

    def foldRight[A, B](l: List[A])(accumulator: B)(f: (A, B) => B): B = l match
        case Cons(h, t) => f(h, foldRight(t)(accumulator)(f))
        case Nil() => accumulator
    
    def printList[A](l: List[A]): Int = l match
      case Cons(h, t) => println(h); printList(t);
      case Nil() => println("Nil"); 0

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
