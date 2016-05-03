package com.exercises.chapter

import com.exercises.chapter.two._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class twoSpec extends FunSuite with Checkers{
  private val smallPosFrom0: Gen[Int] = Gen.choose(0, 100)
  private val smallPosFrom2: Gen[Int] = Gen.choose(2, 100)

  test("Fibonacci if and only if one or both of (5*n2 + 4) or (5*n2 – 4) is a perfect square"){
    val property = forAll(smallPosFrom0) { (int: Int) => {
      val common = 5 * Math.pow(fibonacci(int), 2)
      isPerfectSquare(common + 4) || isPerfectSquare(common - 4)
    }}
    check(property)
  }

  test("Fibonacci - each fibonacci number bigger than 3 is bigger than the previous one") {
    val property =
      forAll(smallPosFrom2){ (int: Int) =>
      fibonacci(int) > fibonacci(int - 1)}
    check(property)
  }

  test("Fibonacci - each fibonacci number bigger than 2 is the sum of the previous two") {
    val property =
      forAll(smallPosFrom2){ (int: Int) =>
      fibonacci(int) == fibonacci(int - 1) + fibonacci(int - 2)}
    check(property)
  }

  def isPerfectSquare(n: Double) = {
    val s = Math.sqrt(n)
    s * s == n
  }
}
