package com.exercises.chapter

import com.exercises.chapter.two._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, WordSpec}

class twoSpec extends BaseSpec {
  private val smallPosFrom0: Gen[Int] = Gen.choose(0, 39)
  private val smallPosFrom2: Gen[Int] = Gen.choose(2, 45)

  " For a Fibonacci number " should {
    " if and only if one or both of (5*n2 + 4) or (5*n2 – 4) is a perfect square" in {
      def isPerfectSquare(n: Double) = {
        val s = Math.sqrt(n)
        s * s == n
      }

      val property = forAll(smallPosFrom0) { (int: Int) => {
        val common = 5 * Math.pow(fibonacci(int), 2)
        isPerfectSquare(common + 4) || isPerfectSquare(common - 4)
      }
      }

      check(property)
    }

    "each fibonacci number bigger than 3 is bigger than the previous one" in {
      val property =
        forAll(smallPosFrom2) { (int: Int) =>
          fibonacci(int) > fibonacci(int - 1)
        }
      check(property)
    }

    "Fibonacci - each fibonacci number bigger than 2 is the sum of the previous two" in {
      val property =
        forAll(smallPosFrom2) { (int: Int) =>
          fibonacci(int) == fibonacci(int - 1) + fibonacci(int - 2)
        }
      check(property)
    }

    "Fibonacci - 0 and 1 are equal to 1" in {
      fibonacci(0) == fibonacci(1)
    }
  }

  "isSorted" should {
    "return true" when {

      "given asc ordered arrays ints and the function <= the function should always" in {
        val genAscOrderedIntArray = Gen.containerOf[Array, Int](Gen.chooseNum(Int.MinValue, Int.MaxValue)).map(_.sorted)
        def minor(a: Int, b: Int) = a <= b
        val property = forAll(genAscOrderedIntArray) { (a: Array[Int]) => isSorted(a, minor) }

        check(property)
      }

      "given desc ordered arrays and the function >= the function" in {
        val genAscOrderedIntArray = Gen.containerOf[Array, Int](Gen.chooseNum(Int.MinValue, Int.MaxValue)).map(_.sorted(Ordering.Int.reverse))
        def minor(a: Int, b: Int) = a >= b
        val property = forAll(genAscOrderedIntArray) { (a: Array[Int]) => isSorted(a, minor) }

        check(property)
      }

      "given arrays with the same int repeated the function < the function " in {
        val genAscOrderedIntArray = for {
          size <- Gen.chooseNum(2, 1000)
          elem <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
        } yield Array.fill(size)(elem)

        def minor(a: Int, b: Int) = a <= b
        val property = forAll(genAscOrderedIntArray) { (a: Array[Int]) => isSorted(a, minor) }

        check(property)
      }
    }

    "return false" when {
      "given asc ordered arrays and the function <= the function" in {
        val genAscOrderedIntArray = Gen.containerOf[Array, Int](Gen.chooseNum(Int.MinValue, Int.MaxValue))
          .suchThat(_.length > 1).map(_.sorted(Ordering.Int.reverse))
        def minor(a: Int, b: Int) = a <= b
        val property = forAll(genAscOrderedIntArray) { (a: Array[Int]) => !isSorted(a, minor) }

        check(property)
      }
    }
  }
}
