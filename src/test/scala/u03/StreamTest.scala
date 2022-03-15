package u03

import org.junit.*
import org.junit.Assert.*
import Streams.*
import u03.Lists.List.*
import u02.Optionals.*
import u02.AlgebraicDataTypes.*

class StreamTest:

  @Test def testDrop() =
    val s = Stream.take(Stream.iterate(0)( _ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))

  @Test def testConstant() =
    assertEquals( Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test def testFibs() =
    val fibs: Stream[Int] = Stream.fibsStream()
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13 , Nil())))))))), Stream.toList(Stream.take(fibs)(8)))