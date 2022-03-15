package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.Optionals.*
import u02.AlgebraicDataTypes.*

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend() =
    val tail = Cons (40 , Nil ())
    assertEquals( Cons(10, Cons(20, Cons(30, Cons(40,Nil())))), append(l, tail))

  @Test def testFlatMap() =
    assertEquals(Cons (11 , Cons(21 , Cons(31 , Nil ()))), flatMap(l)(v => Cons(v + 1, Nil())) )
    assertEquals( Cons (11 , Cons(12 , Cons(21 , Cons(22 , Cons(31 , Cons (32 , Nil())))))) ,flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil() ))))

  @Test def testMapwithFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFlatMap(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFlatMap(l)(_ + ""))

  @Test def testFilterWithFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFlatMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFlatMap(l)(_ != 20))

  @Test def testMax() =
    assertEquals(Option.Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(Option.None(), max(Nil()))

  @Test def testGetTeacherCourses() =
    assertEquals(Cons("OS", Cons("PPS", Nil())), getTeacherCourses(Cons(Person.Student("Davide", 23), Cons(Person.Teacher("Ghini", "OS"), Cons(Person.Student("Simone", 22), Cons(Person.Teacher("Viroli", "PPS"), Nil()))))))
    assertEquals(Cons("OS", Cons("PPS", Nil())), getTeacherCoursesWithFlatMap(Cons(Person.Student("Davide", 23), Cons(Person.Teacher("Ghini", "OS"), Cons(Person.Student("Simone", 22), Cons(Person.Teacher("Viroli", "PPS"), Nil()))))))

