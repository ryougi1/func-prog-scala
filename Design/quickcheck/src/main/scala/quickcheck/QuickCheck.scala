package quickcheck

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  /**
    * Here are some basic generators that you can combine together to create
    * larger ones:
    *
    * arbitrary[T] is a generator that generates an arbitrary value of type T.
    * As we are interested in IntHeaps it will generate arbitrary integer
    * values, uniformly at random.
    * oneOf(gen1, gen2) is a generator that picks one of gen1 or gen2,
    * uniformly at random.
    * const(v) is a generator that will always return the value v.
    * choose(v1, v2) is a generator that picks any value v1 < v < v2
    * frequency((2, gen1),(3, gen2)) is like oneOf, but with distribution
    */
  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (6, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /**
    * https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md
    *
    * A property is the testable unit in ScalaCheck, and is represented by the
    * org.scalacheck.Prop class. There are several ways to create properties in
    * ScalaCheck, one of them is to use the org.scalacheck.Prop.forAll method
    * like in the example above. That method creates universally quantified
    * properties directly, but it is also possible to create new properties by
    * combining other properties, or to use any of the specialised methods in
    * the org.scalacheck.Prop object.
    *
    * ... Generators ...
    *
    * We can test properties or property collections by using the check method.
    * The Test module is responsible for all test execution in ScalaCheck. It
    * will generate the arguments and evaluate the properties, repeatedly with
    * larger and larger test data (by increasing the size parameter used by the
    * generators). If it doesn't manage to find a failing test case after a
    * certain number of tests, it reports a property as passed.
    *
    */

  /**
    * Adding a single element to an empty heap, and then removing this element,
    * should yield the element in question.
    */
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * For any heap, adding the minimal element, and then finding it, should
    * return the element in question
    */
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * If you insert any two elements into an empty heap, finding the minimum of
    * the resulting heap should get the smallest of the two elements back.
    */
  property("min of meld is min, single element") = forAll { (a: A, b: A) =>
    findMin(insert(b, insert(a, empty))) == min(a, b)
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum, the
    * resulting heap should be empty.
    */
  property("heap is empty after insert and delete") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when
    * continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("heap is sorted when continually finding and deleting minima") =
    forAll { (h: H) =>
      def isSorted(h: H): Boolean =
        if (isEmpty(h)) true
        else {
          val min = findMin(h)
          val rest = deleteMin(h)
          isEmpty(rest) || (min <= findMin(rest) && isSorted(rest))
        }

      isSorted(h)
    }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum
    * of one or the other.
    */
  property("min of meld is min of melded heaps pre-meld") = forAll {
    (h1: H, h2: H) =>
      meld(h1, h2) match {
        case _: H => {
          if (isEmpty(meld(h1, h2))) true
          else if (isEmpty(h1)) findMin(meld(h1, h2)) == findMin(h2)
          else if (isEmpty(h2)) findMin(meld(h1, h2)) == findMin(h1)
          else min(findMin(h1), findMin(h2)) == findMin(meld(h1, h2))
        }
        case _ => false
      }
  }

  property("meld of two heaps is the same as meld of same heaps") = forAll {
    (h1: H, h2: H) =>
      def areEqual(h1: H, h2: H): Boolean =
        if (isEmpty(h1) && isEmpty(h2)) true
        else {
          findMin(h1) == findMin(h2) && areEqual(deleteMin(h1), deleteMin(h2))
        }

      val meld1 = meld(h1, h2)
      val meld2 = meld(insert(findMin(h1), h2), deleteMin(h1))
      areEqual(meld1, meld2)
  }
}
