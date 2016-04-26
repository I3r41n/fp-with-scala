package com.exercises.chapter

import org.scalatest.{FunSuite, WordSpec}
import org.scalacheck.Gen.posNum
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalacheck.Test.check
import com.exercises.chapter.two._


class twoSpec extends FunSuite with Checkers{


  test("Fibonacci - each fibonacci number bigger than 1 is the sum of the previous two") {
    val property = forAll(posNum[Int] suchThat ( _ > 1)){ (int: Int) =>
      fibonacci(int) == fibonacci(int - 1) + fibonacci(int - 2)}
    check(property)
  }
}
