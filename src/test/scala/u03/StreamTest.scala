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
