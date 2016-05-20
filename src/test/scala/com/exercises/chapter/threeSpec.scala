package com.exercises.chapter

import org.scalacheck.{Arbitrary, Gen}
import fpinscala.datastructures.{List => MyList, _}
class threeSpec extends BaseSpec {
  implicit def myListArbitrary[T:Arbitrary]: Arbitrary[MyList[T]] = Arbitrary[MyList[T]](Gen.oneOf(myNilGen, myConsGen[T]))

  private def myConsGen[T: Arbitrary]: Gen[MyList[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Gen.oneOf(myNilGen, myConsGen[T])
  } yield Cons(head , tail )

  private val myNilGen: Gen[MyList[Nothing]] = Gen.delay(Nil)

  "List Pattern Matching " should {
    "return 3 " in {
      val res = three.ListPatternMatching
      res should be(3)
    }
  }

  "List tail " should {
    "return a list without the first element of the original list" in {
//       println(myConsGen[Int].sample)
    }
  }

}
