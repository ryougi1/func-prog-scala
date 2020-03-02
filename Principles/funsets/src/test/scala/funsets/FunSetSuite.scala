package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s100 = (x: Int) => x >= -100 && x <= 100
    val s1000 = (x: Int) => x >= -1000 && x <= 1000
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   *
   * @Ignore annotation.
   */
  //  @Ignore("not ready yet") @Test def `singleton set one contains one`: Unit = {

  @Test def `singleton set one contains one`: Unit = {
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains intersects of each set`: Unit = {
    new TestSets {
      val s = intersect(union(s1, s2), s1)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  @Test def `diff returns all elements of first set that are not in second set`: Unit = {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s = diff(s12, s23)

      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  @Test def `filter returns subset of s for which p holds`: Unit = {
    new TestSets {
      val s12 = union(s1, s2)
      val s = filter(s12, (x: Int) => x % 2 == 0)
      assert(contains(s, 2), "Filter 2")
      assert(!contains(s, 1), "Filter 3")
    }
  }

  @Test def `forall returns whether all bounded ints within s satisfy p`: Unit = {
    new TestSets {
      assert(forall(s1000, (x: Int) => x != 9999), "Forall 1")
      assert(!forall(s1000, (x: Int) => x != 0), "Forall 2")
      assert(forall(s100, (x: Int) => x != 9999), "Forall 3")
    }
  }

  @Test def `exists returns whether there exists a bounded integer within s that satisfies p`: Unit = {
    new TestSets {
      assert(exists(s1000, (x: Int) => x % 11 == 0))
      assert(!exists(s100, (x: Int) => x == 1000))
    }
  }

  @Test def `map returns a set transformed by appling f to each element of s`: Unit = {
    new TestSets {
      var sm1 = map(s100, (x: Int) => x * 2)
      var sm2 = map(s100, (x: Int) => x + 1)
      assert(contains(sm1, 200))
      assert(!contains(sm1, 199))
      assert(contains(sm2, 101))
      assert(!contains(sm2, -100))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
